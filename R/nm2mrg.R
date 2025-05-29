#' Convert NONMEM model to mrgsolve format
#'
#' This function reads a NONMEM control stream and converts it into an mrgsolve-compatible model specification.
#'
#' @param mod_name A character string specifying the model name (run number) without extension.
#' @param dir A character string specifying the directory where the NONMEM control stream file is located.
#' @param use_final A logical value indicating whether to use the final estimates for parameters, omega, and sigma. If TRUE, the function will use the final estimates from the NONMEM output files.
#'
#' @return A character string representing the mrgsolve model file content.
#' @export
nm2mrg <- function(mod_name, dir = "./", use_final = FALSE, add_CAPTURE = TRUE) {
  tmp_mod <- xpose::read_nm_model(runno = mod_name, prefix = "", dir = dir, ext = ".mod")
  tmp_mod <- tmp_mod[tmp_mod$code != "", ]

  mrg_mod <- list()

  tmp_prob <- tmp_mod[tmp_mod$subroutine == "pro", "code"]
  tmp_prob <- ifelse(nrow(tmp_prob) == 0, "", tmp_prob)
  mrg_mod$prob <- paste0("$PROB ", tmp_prob, "\n")


  mrg_mod$plugin <- "$PLUGIN autodec nm-vars\n"


  if (use_final) {
    tmp_theta <- get_finalestimate_theta(mod_name, dir)
  } else {
    tmp_theta <- "$THETA @annotated\n"
    for (i in 1:nrow(tmp_mod[tmp_mod$subroutine == "the", ])) {
      tmp_theta_row <- tmp_mod[tmp_mod$subroutine == "the", ][i, ]
      theta <- extract_param(tmp_theta_row$code)
      theta <- gsub("FIX|FIXED", "", theta)
      theta <- paste0(theta, " : ", tmp_theta_row$comment)
      tmp_theta <- paste0(tmp_theta, theta, "\n")
    }
  }
  mrg_mod$theta <- tmp_theta


  tmp_cov_names <- extract_undefined_variable(tmp_mod[tmp_mod$subroutine %in% c("pk", "des"), ]$code)
  if (length(tmp_cov_names) > 0) {
    warning("Some covariates were detected. The initial value is set to 1 by default. Please update it as needed.")
    tmp_cov <- paste0(tmp_cov_names, " = 1\n")
    tmp_cov <- paste0(tmp_cov, collapse = "")
    mrg_mod$cov <- paste0("$PARAM @covariates\n", tmp_cov)
  }

  tmp_cmt <- apply(tmp_mod[tmp_mod$subroutine == "mod", "code"], 1, function(x) gsub("COMP|=|\\(|\\)| ", "", x))
  tmp_cmt <- paste(tmp_cmt, collapse = "\n")
  mrg_mod$cmt <- paste0("$CMT\n", tmp_cmt, "\n")


  tmp_pk <- sapply(tmp_mod[tmp_mod$subroutine == "pk", ]$code, replace_pow_from_string, USE.NAMES = FALSE)
  tmp_pk <- sapply(tmp_pk, convert_if_line, USE.NAMES = FALSE)
  tmp_pk <- sapply(tmp_pk, add_semicolon, USE.NAMES = FALSE)
  tmp_pk <- paste0(tmp_pk, collapse = "\n")
  mrg_mod$pk <- paste0("$PK\n", tmp_pk, "\n")


  if (use_final) {
    tmp_omega <- get_finalestimate_omega(mod_name, dir)
  } else {
    tmp_omega <- ""
    omega_counter <- 0
    f_omega_header <- TRUE
    for (i in 1:nrow(tmp_mod[tmp_mod$subroutine == "ome", ])) {
      tmp_omega_code <- tmp_mod[tmp_mod$subroutine == "ome", "code"][i, ]
      tmp_omega_code <- apply(tmp_omega_code, 1, function(x) gsub("FIX|FIXED", "", x))
      if (grepl("BLOCK", tmp_omega_code)) {
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
  }
  mrg_mod$omega <- tmp_omega


  if (use_final) {
    tmp_sigma <- get_finalestimate_sigma(mod_name, dir)
  } else {
    tmp_sigma <- tmp_mod[tmp_mod$subroutine == "sig", "code"]
    tmp_sigma <- apply(tmp_sigma, 1, function(x) gsub("FIX|FIXED| ", "", x))
    tmp_sigma <- paste0(tmp_sigma, collapse = "\n")
    tmp_sigma <- paste0("$SIGMA\n", tmp_sigma, "\n")
  }
  mrg_mod$sigma <- tmp_sigma


  tmp_des <- sapply(tmp_mod[tmp_mod$subroutine == "des", ]$code, replace_pow_from_string, USE.NAMES = FALSE)
  tmp_des <- sapply(tmp_des, convert_if_line, USE.NAMES = FALSE)
  tmp_des <- sapply(tmp_des, add_semicolon, USE.NAMES = FALSE)
  tmp_des <- paste0(tmp_des, collapse = "\n")
  mrg_mod$des <- paste0("$DES\n", tmp_des, "\n")


  tmp_error <- sapply(tmp_mod[tmp_mod$subroutine == "err", ]$code, replace_pow_from_string, USE.NAMES = FALSE)
  tmp_error <- sapply(tmp_error, convert_if_line, USE.NAMES = FALSE)
  tmp_error <- sapply(tmp_error, add_semicolon, USE.NAMES = FALSE)
  tmp_error <- paste0(tmp_error, collapse = "\n")
  mrg_mod$error <- paste0("$ERROR\n", tmp_error, "\n")

  if (add_CAPTURE) {
    tmp_capt <- "$CAPTURE\nEVID CMT AMT"
    if (length(tmp_cov_names) > 0) {
      tmp_capt <- paste(c(tmp_capt, tmp_cov_names), collapse = " ")
    }
    mrg_mod$capt <- tmp_capt
  }

  return(paste0(c(mrg_mod, ""), collapse = "\n"))
}


extract_param <- function(prm_string) {
  extracted_params <- gsub("\\(|\\)| ", "", prm_string)
  extracted_params <- strsplit(extracted_params, ",")
  extracted_params <- unlist(extracted_params)
  extracted_params <- switch(length(extracted_params),
    "1" = extracted_params[1],
    "2" = extracted_params[2],
    "3" = extracted_params[2]
  )
  return(extracted_params)
}


extract_undefined_variable <- function(code_lines) {
  assign_lines <- grep("(?<![=!<>])=(?![=])", code_lines, value = TRUE, perl = TRUE)
  lhs_vars <- gsub("=.*", "", assign_lines)
  lhs_vars <- trimws(lhs_vars)

  full_text <- paste(code_lines, collapse = "\n")
  all_vars <- unlist(regmatches(full_text, gregexpr("\\b[A-Za-z_][A-Za-z0-9_]*\\b", full_text)))

  rhs_vars <- setdiff(all_vars, lhs_vars)

  reserved_vars <- c("THETA", "ETA", "EXP", "LOG", "SQRT", "ABS", "SIN", "COS",
                      "IF", "THEN", "ELSE", "ENDIF", "T", "AND", "OR",
                      "EQ", "NE", "LE", "LT", "GE", "GT", "A", "DADT", "A_0")
  undefined_vars <- setdiff(rhs_vars, reserved_vars)

  undefined_vars <- undefined_vars[!grepl("^F[0-9.]+$", undefined_vars)]
  undefined_vars <- undefined_vars[!grepl("^R[0-9.]+$", undefined_vars)]
  undefined_vars <- undefined_vars[!grepl("^S[0-9.]+$", undefined_vars)]
  undefined_vars <- undefined_vars[!grepl("^D[0-9.]+$", undefined_vars)]

  return(unique(undefined_vars))
}


replace_pow_from_string <- function(expr_str) {
  if (grepl("(?i)if|else", expr_str)) {
    return(expr_str)
  }

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


convert_if_line <- function(line) {
  line <- convert_operators(line)

  line <- gsub("(?i)^\\s*if\\s*\\((.*)\\)(.*)?$", "if (\\1)\\2", line)
  line <- gsub("(?i)\\s*then$", " {", line)
  line <- gsub("(?i)^\\s*else if\\s*\\((.*)\\)\\s*\\{?$", "} else if (\\1) {", line)
  line <- gsub("(?i)^else$", "} else {", line)
  line <- gsub("(?i)^endif$", "}", line)

  return(line)
}


convert_operators <- function(line) {
  line <- gsub("(?i)\\.eq\\.", "==", line)
  line <- gsub("(?i)\\.ne\\.", "!=", line)
  line <- gsub("(?i)\\.gt\\.", ">", line)
  line <- gsub("(?i)\\.ge\\.", ">=", line)
  line <- gsub("(?i)\\.lt\\.", "<", line)
  line <- gsub("(?i)\\.le\\.", "<=", line)

  return(line)
}


add_semicolon <- function(line) {
  if (grepl("(?i)if|else|\\{|\\}", line)) {
    return(line)
  } else {
    return(paste0(line, ";"))
  }
}
