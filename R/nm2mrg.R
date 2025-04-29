#' Convert NONMEM model to mrgsolve format
#'
#' This function reads a NONMEM control stream and converts it into an mrgsolve-compatible model specification.
#'
#' @param mod_name A character string specifying the model name (run number) without extension.
#' @param dir A character string specifying the directory where the NONMEM control stream file is located.
#'
#' @return A character string representing the mrgsolve model file content.
#' @export
nm2mrg <- function(mod_name, dir = "./") {

  tmp_mod <- xpose::read_nm_model(runno = mod_name, prefix = "", dir = dir, ext = ".mod")
  tmp_mod <- tmp_mod[tmp_mod$code != "",]

  mrg_mod <- list()

  mrg_mod$prob <- paste("$PROB", tmp_mod[tmp_mod$subroutine == "pro", "code"], "\n")


  mrg_mod$plugin <- "$PLUGIN autodec nm-vars\n"


  if (any(tmp_mod[tmp_mod$subroutine == "the", "comment"] != "")) {
    tmp_param <- "$PARAM @annotated\n"
    f_the_anno <- TRUE
  } else {
    tmp_param <- "$PARAM\n"
    f_the_anno <- FALSE
  }

  for (i in 1:nrow(tmp_mod[tmp_mod$subroutine == "the", ])) {
    tmp_theta <- tmp_mod[tmp_mod$subroutine == "the", ][i,]
    param <- extract_param(tmp_theta$code)
    param <- paste0("THETA", i, " : ", param)
    if (f_the_anno) {
      param <- paste0(param, " : ", tmp_theta$comment)
    }
    tmp_param <- paste0(tmp_param, param, "\n")
  }
  mrg_mod$param <- tmp_param


  tmp_cmt <- apply(tmp_mod[tmp_mod$subroutine == "mod", "code"], 1, function(x)gsub("COMP|=|\\(|\\)| ", "", x))
  tmp_cmt <- paste(tmp_cmt, collapse = "\n")
  mrg_mod$cmt <- paste0("$CMT\n", tmp_cmt, "\n")


  tmp_pk <- sapply(tmp_mod[tmp_mod$subroutine == "pk",]$code, replace_pow_from_string, USE.NAMES = FALSE)
  tmp_pk <- paste0(tmp_pk, collapse = ";\n")
  mrg_mod$pk <- paste0("$PK\n", tmp_pk, ";\n")


  tmp_omega <- ""
  omega_counter <- 0
  f_omega_header <- TRUE
  for (i in 1:nrow(tmp_mod[tmp_mod$subroutine == "ome",])) {
    tmp_omega_code <- tmp_mod[tmp_mod$subroutine == "ome", "code"][i,]
    if (grepl("BLOCK", tmp_omega_code)){
      omega_counter <- gsub("BLOCK|\\(|\\)", "", tmp_omega_code)
      omega_counter <- as.numeric(omega_counter)
      tmp_omega <- paste0(tmp_omega, "$OMEGA @block\n")
      f_omega_header <- FALSE
      next()
    }
    if (omega_counter == 0 & f_omega_header) {
      tmp_omega <- paste0(tmp_omega, "$OMEGA\n")
      f_omega_header <- FALSE
    }

    tmp_omega <- paste0(tmp_omega, tmp_omega_code, "\n")

    if (omega_counter != 0) {
      omega_counter <- omega_counter - 1
      if (omega_counter == 0) {
        f_omega_header <- TRUE
      }
    }
  }
  mrg_mod$omega <- tmp_omega


  tmp_sigma <- tmp_mod[tmp_mod$subroutine == "sig", "code"]
  tmp_sigma <- apply(tmp_sigma, 1, function(x)gsub("FIX| ", "", x))
  tmp_sigma <- paste0(tmp_sigma, collapse = "\n")
  mrg_mod$sigma <- paste0("$SIGMA\n", tmp_sigma, "\n")


  tmp_des <- sapply(tmp_mod[tmp_mod$subroutine == "des",]$code, replace_pow_from_string, USE.NAMES = FALSE)
  tmp_des <- paste0(tmp_des, collapse = ";\n")
  mrg_mod$des <- paste0("$DES\n", tmp_des, ";\n")


  tmp_error <- sapply(tmp_mod[tmp_mod$subroutine == "err",]$code, replace_pow_from_string, USE.NAMES = FALSE)
  tmp_error <- paste0(tmp_error, collapse = ";\n")
  mrg_mod$error <- paste0("$ERROR\n", tmp_error, ";\n")

  return(paste(mrg_mod, collapse = "\n"))
}


extract_param <- function(prm_string) {
  extracted_params <- gsub("\\(|\\)| ", "", prm_string)
  extracted_params <- strsplit(extracted_params, ",")
  extracted_params <- unlist(extracted_params)
  extracted_params <- switch(length(extracted_params),
    "1" = extracted_params[1],
    "2" = extracted_params[2],
    "3" = extracted_params[2])
  return(extracted_params)
}


replace_pow_from_string <- function(expr_str) {
  expr <- parse(text = expr_str)[[1]]

  replace_pow <- function(expr) {
    if (is.call(expr)) {
      op <- as.character(expr[[1]])
      if (op %in% c("^")) {
        return(as.call(list(
          as.name("pow"),
          replace_pow(expr[[2]]),
          replace_pow(expr[[3]])
        )))
      } else {
        return(as.call(lapply(expr, replace_pow)))
      }
    } else if (is.pairlist(expr)) {
      return(as.pairlist(lapply(expr, replace_pow)))
    } else {
      return(expr)
    }
  }

  paste(deparse(replace_pow(expr)), collapse = "")
}
