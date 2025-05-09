get_finalestimate_theta <- function(mod_name, dir) {
  xpdb <- xpose::xpose_data(mod_name, prefix = "", dir = dir)
  tmp_the <- xpose::get_prm(xpdb, transform = FALSE)
  tmp_the <- tmp_the[tmp_the$type == "the", ]

  theta_header <- "$THETA @annotated\n"
  output <- paste0(tmp_the$value, " : ", tmp_the$label, "\n", collapse = "")

  return(paste0(theta_header, output))
}

get_finalestimate_omega <- function(mod_name, dir) {
  xpdb <- xpose::xpose_data(mod_name, prefix = "", dir = dir)
  tmp_ome <- xpose::get_prm(xpdb, transform = FALSE)
  tmp_ome <- tmp_ome[tmp_ome$type == "ome", ]

  off_diag <- tmp_ome[!tmp_ome$diagonal, ]
  m_ <- off_diag$m
  n_ <- off_diag$n

  tmp_ome$block <- FALSE

  for (i in seq_along(m_)) {
    lo <- min(m_[i], n_[i])
    hi <- max(m_[i], n_[i])
    idx <- tmp_ome$m >= lo & tmp_ome$m <= hi & tmp_ome$n >= lo & tmp_ome$n <= hi
    tmp_ome$block[idx] <- TRUE
  }

  rle_block <- rle(tmp_ome$block)
  group <- rep(seq_along(rle_block$lengths), rle_block$lengths)
  tmp_ome$group <- group

  overall_output <- ""

  group_ids <- unique(tmp_ome$group)

  for (i in group_ids) {
    group_df <- tmp_ome[tmp_ome$group == i, ]

    is_block <- group_df$block[1]
    if (is_block) {
      ome_header <- "$OMEGA @block\n"
    } else {
      ome_header <- "$OMEGA\n"
    }

    m_vals <- unique(group_df$m)
    lines <- character()

    for (m_val in m_vals) {
      row_df <- group_df[group_df$m == m_val, ]
      row_df <- row_df[order(row_df$n), ]

      value_str <- paste(row_df$value, collapse = " ")
      line <- paste0(value_str, "\n")
      lines <- c(lines, line)
    }

    output <- paste0(ome_header, paste(lines, collapse = ""))
    overall_output <- paste0(overall_output, output)
  }

  return(overall_output)
}


get_finalestimate_sigma <- function(mod_name, dir) {
  xpdb <- xpose::xpose_data(mod_name, prefix = "", dir = dir)
  tmp_sig <- xpose::get_prm(xpdb, transform = FALSE)
  tmp_sig <- tmp_sig[tmp_sig$type == "sig", ]

  sigma_header <- "$SIGMA\n"
  output <- paste0(tmp_sig$value, "\n", collapse = "")

  return(paste0(sigma_header, output))
}
