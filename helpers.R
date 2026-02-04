# Pseudo R-square -----------------------------------------------------------
r2_pseudo <- function(mf, mr, mnull = mr) {
  V_full <- .V_table(mf)
  V_restricted <- .V_table(mr)
  V_empty <- .V_table(mnull)

  V_full |>
    dplyr::inner_join(V_restricted, by = c("grp", "var")) |>
    dplyr::inner_join(V_empty, by = c("grp", "var")) |>
    dplyr::mutate(
      r2 = (vcov.y - vcov.x) / vcov,
    ) |>
    dplyr::select(grp, var, r2)
}

## Variance-covariance table extraction ----------------------------------------

.V_table <- function(model) {
  UseMethod(".V_table")
}

.V_table.lm <- function(model) {
  tibble::tibble(
    grp = "Residual",
    var = NA,
    vcov = sigma(model)^2
  )
}

.V_table.glm <- .V_table.lm

.V_table.lmerMod <- function(model) {
  tibble::as_tibble(lme4::VarCorr(model)) |>
    dplyr::filter(is.na(var2)) |>
    dplyr::select(-sdcor, -var2) |>
    dplyr::rename(var = var1)
}

.V_table.glmerMod <- function(model) {
  res <- tibble::tibble(
    grp = "Residual",
    var = NA,
    vcov = sigma(model)^2
  )

  .V_table.lmerMod(model) |>
    dplyr::bind_rows(res)
}

.V_table.stanreg <- .V_table.lmerMod

.V_table.brmsfit <- function(model) {
  V <- lme4::VarCorr(model)

  purrr::imap(V, \(v, grp) {
    x <- v[["sd"]][, "Estimate"]
    tibble::tibble(
      grp = ifelse(grp == "residual__", "Residual", grp),
      var = names(x),
      vcov = x^2
    )
  }) |>
    purrr::list_rbind()
}

.V_table.glmmTMB <- function(model) {
  V <- VarCorr(model)

  L <- purrr::imap(V[["cond"]], \(v, grp) {
    x <- diag(v)
    tibble::tibble(
      grp = grp,
      var = names(x),
      vcov = x
    )
  })

  L |>
    append(
      list(tibble::tibble(grp = "Residual", vcov = attr(V[["cond"]], "sc")))
    ) |>
    purrr::list_rbind()
}

.V_table.default <- function(model) {
  f <- ls(all.names = TRUE, pattern = "^\\.V_table\\.", envir = .GlobalEnv)
  cls <- sub("^\\.V_table\\.", "", f)
  stop(
    sprintf(
      "No method for objects of class '%s'\n\nSupported classes:\n%s",
      class(model)[1],
      paste0(cls, collapse = ", ")
    ),
    call. = FALSE
  )
}
