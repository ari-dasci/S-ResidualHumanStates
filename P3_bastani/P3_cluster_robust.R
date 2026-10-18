# =====================================================================
# P3 FROZEN ANALYSIS - Bastani et al. (2025, PNAS)
# Source: https://github.com/obastani/GenAICanHarmLearning
#   commit 2f63dae1a01d51453826fe07ef5cf6678e339588 (2025-06-25)
#   file   main_regressions/final_data.csv ; run from main_regressions/
# Packages: readr, sandwich, clubSandwich, fwildclusterboot (0.14.3, GitHub
#   s3alfisc/fwildclusterboot commit 336bb574, 2023-09-12), dqrng
#
# FROZEN INFERENCE SPECIFICATION (do not change after freezing)
#  - Models: original specification, lm(), Honors == 0, listwise deletion
#  - CR1: sandwich::vcovCL(type = "HC1"), clusters = Class, t(G-1 = 43)
#  - CR2: clubSandwich (CR2, Satterthwaite df), contrasts by coefficient name
#  - PRIMARY wild cluster restricted bootstrap (manuscript Table 4):
#      fwildclusterboot::boottest, engine = "R", bootstrap_type = "fnw11"
#      (WCR11), type = "rademacher", impose_null = TRUE, p_val_type =
#      "two-tailed", default ssc, clustid = "Class", B = 9999,
#      Tutor-Base via param = c("GPTTutor","GPTBase"), R = c(1,-1);
#      set.seed(20260928) AND dqrng::dqset.seed(20260928) before EACH
#      contrast (order-independent).
#  - Cross-check: own WCR implementation (not reported in the manuscript):
#      * null imposed: restricted model estimated under H0
#          (single coefficient dropped; Tutor-Base via GPTBase+GPTTutor
#           collapsed into one regressor, i.e. beta_T = beta_B imposed)
#      * weights: Rademacher (+1/-1, p = 1/2), one draw per classroom
#      * statistic: t = L'b / sqrt(L' V L), V = CR1 with small-sample factor
#          G/(G-1) * (n-1)/(n-k)
#      * p-value: two-sided symmetric, share of |t*| >= |t|
#      * B = 4999 replications
#      * RNG: base R default (Mersenne-Twister, Inversion, Rejection),
#          set.seed(20260928) once; contrasts run in the fixed order of
#          `est` below (outcome varies fastest within estimand)
# =====================================================================
suppressPackageStartupMessages({library(readr); library(sandwich); library(clubSandwich); library(fwildclusterboot); library(dqrng)})
RNGkind("Mersenne-Twister", "Inversion", "Rejection")   # R default; set BEFORE the seed
set.seed(20260928)
BASTANI_DIR <- Sys.getenv("BASTANI_DIR", "data/GenAICanHarmLearning/main_regressions")
OUT <- Sys.getenv("OUT_DIR", "output/P3"); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
df <- read_csv(file.path(BASTANI_DIR, "final_data.csv"), show_col_types = FALSE); df <- df[df$Honors == 0, ]
for (v in c("teacher", "Session", "Year", "Grader")) df[[v]] <- factor(df[[v]])

# ---- 0. Sample and design checks (on the rows actually used by each model)
for (y in c("Part2Tot", "Part3Tot")) {
  m0 <- lm(as.formula(paste(y, "~ GPTBase + GPTTutor + gpa_prev + teacher + Session + Grader + Year")), data = df)
  used <- df[match(rownames(model.frame(m0)), rownames(df)), ]
  arms <- unique(used[, c("Class", "Treatment arm")])
  stopifnot(nrow(m0$model) == 2848,
            length(unique(used$Class)) == 44,
            !anyDuplicated(arms$Class))                 # each class in one arm
  print(table(arms$`Treatment arm`))                   # expected: control 17, vanilla 13, augmented 14
  stopifnot(all(table(arms$`Treatment arm`)[c("control", "vanilla", "augmented")] == c(17, 13, 14)))
  b <- coef(m0)[c("GPTBase", "GPTTutor")]
  target <- if (y == "Part2Tot") c(0.1370, 0.3613) else c(-0.0543, -0.0043)
  stopifnot(all(abs(round(b, 4) - target) < 1e-4))     # published coefficients to rounding
  cat(y, ": n =", nrow(m0$model), "| coefficients reproduce\n")
}
f_main <- function(y, rhs = "GPTBase + GPTTutor + gpa_prev + teacher + Session + Grader + Year")
  as.formula(paste(y, "~", rhs))
Lvec <- function(m, which) { L <- setNames(rep(0, length(coef(m))), names(coef(m)))
  if (which == "T-B") { L["GPTTutor"] <- 1; L["GPTBase"] <- -1 } else L[which] <- 1; L }

# 1. Original CR1 (as published) with df = G-1 = 43
cr1 <- function(y, which, rhs = NULL) {
  m <- lm(if (is.null(rhs)) f_main(y) else f_main(y, rhs), data = df)
  cl <- df$Class[match(rownames(model.frame(m)), rownames(df))]
  V <- vcovCL(m, cluster = cl); L <- Lvec(m, which)
  e <- sum(L * coef(m)); s <- sqrt(as.numeric(t(L) %*% V %*% L))
  data.frame(outcome = y, estimand = which, est = e, se = s,
             lo = e - qt(.975, 43) * s, hi = e + qt(.975, 43) * s, p = 2 * pt(-abs(e / s), 43))
}
# Values quoted in the manuscript text: CR1 Tutor-Base contrasts with t(43) CIs, and the
# normal-reference p-value implied by the original table for GPT Base on the exam
print(rbind(cr1("Part2Tot", "T-B"), cr1("Part3Tot", "T-B")), digits = 4)
x <- cr1("Part3Tot", "GPTBase"); cat("GPT Base exam, normal-reference p =", 2 * pnorm(-abs(x$est / x$se)), "\n")

# 2. CR2 + Satterthwaite (Bell-McCaffrey; Pustejovsky-Tipton)
cr2 <- function(y, which) {
  m <- lm(f_main(y), data = df); cl <- df$Class[match(rownames(model.frame(m)), rownames(df))]
  C <- matrix(Lvec(m, which), 1); colnames(C) <- names(coef(m))
  r <- linear_contrast(m, vcov = "CR2", cluster = cl, contrasts = C, test = "Satterthwaite")
  data.frame(outcome = y, estimand = which, est = r$Est, se = r$SE, df = r$df,
             lo = r$CI_L, hi = r$CI_U, p = 2 * pt(-abs(r$Est / r$SE), r$df))
}
# 3. Wild cluster restricted bootstrap (frozen specification in header)
wcr <- function(y, which, B = 4999) {
  m <- lm(f_main(y), data = df); X <- model.matrix(m); Y <- model.response(model.frame(m))
  cl <- df$Class[match(rownames(model.frame(m)), rownames(df))]; gi <- match(cl, unique(cl)); G <- max(gi)
  L <- Lvec(m, which); n <- nrow(X); k <- ncol(X); cadj <- G / (G - 1) * (n - 1) / (n - k)
  XtXi <- chol2inv(qr.R(qr(X))); Q <- qr(X)
  tstat <- function(Yv) { b <- qr.coef(Q, Yv); e <- Yv - X %*% b
    S <- rowsum(X * as.vector(e), gi); V <- cadj * XtXi %*% crossprod(S) %*% XtXi
    as.numeric(sum(L * b) / sqrt(t(L) %*% V %*% L)) }
  Xr <- X
  if (which == "T-B") { Xr[, "GPTBase"] <- X[, "GPTBase"] + X[, "GPTTutor"]; Xr <- Xr[, colnames(Xr) != "GPTTutor"] }
  else Xr <- Xr[, colnames(Xr) != which]
  br <- qr.coef(qr(Xr), Y); br[is.na(br)] <- 0; fr <- Xr %*% br; er <- Y - fr
  t0 <- tstat(Y); tb <- replicate(B, tstat(fr + er * sample(c(-1, 1), G, TRUE)[gi]))
  data.frame(outcome = y, estimand = which, t = t0, p_wcr = mean(abs(tb) >= abs(t0)))
}

# 3b. PRIMARY bootstrap: fwildclusterboot (frozen specification in header)
wcr_fw <- function(y, which, B = 9999, seed = 20260928) {
  d <- as.data.frame(df[complete.cases(df[, c(y, "GPTBase","GPTTutor","gpa_prev","teacher","Session","Grader","Year","Class")]), ])
  assign("dat_fw", d, envir = .GlobalEnv)            # boottest re-evaluates the lm call
  m <- eval(bquote(lm(.(f_main(y)), data = dat_fw)), envir = .GlobalEnv); stopifnot(nobs(m) == 2848)
  if (which == "T-B") { param <- c("GPTTutor", "GPTBase"); R <- c(1, -1) } else { param <- which; R <- 1 }
  set.seed(seed); dqrng::dqset.seed(seed)
  bt <- boottest(m, param = param, R = R, B = B, clustid = "Class", type = "rademacher",
                 impose_null = TRUE, p_val_type = "two-tailed", bootstrap_type = "fnw11",
                 engine = "R", conf_int = TRUE)
  data.frame(outcome = y, estimand = which, t_fw = bt$t_stat, p_WCR = bt$p_val,
             ci_lo_WCR = bt$conf_int[1], ci_hi_WCR = bt$conf_int[2])
}

est <- expand.grid(outcome = c("Part2Tot", "Part3Tot"), estimand = c("GPTBase", "GPTTutor", "T-B"),
                   stringsAsFactors = FALSE)
res <- do.call(rbind, Map(function(y, w) {
  a <- cr1(y, w); b <- cr2(y, w); c <- wcr(y, w)
  data.frame(outcome = y, estimand = w, est = a$est, se_CR1 = a$se, p_CR1_df43 = a$p,
             se_CR2 = b$se, df_Satt = b$df, lo_CR2 = b$lo, hi_CR2 = b$hi, p_CR2 = b$p,
             p_WCR_own = c$p_wcr)
}, est$outcome, est$estimand))
fw <- do.call(rbind, Map(wcr_fw, est$outcome, est$estimand))
res <- merge(res, fw, by = c("outcome", "estimand"), sort = FALSE)
print(res, digits = 3)
# Holm across the six contrasts (sensitivity reported in the manuscript)
res$p_CR2_holm <- p.adjust(res$p_CR2, "holm"); res$p_WCR_holm <- p.adjust(res$p_WCR, "holm")
print(res[, c("outcome", "estimand", "p_CR2", "p_CR2_holm", "p_WCR", "p_WCR_holm")], digits = 3)
write.csv(res, file.path(OUT, "P3_robustness_inference.csv"), row.names = FALSE)

# 4. Sensitivity of the exam Tutor-Base contrast to fixed effects
specs <- c(full = "GPTBase + GPTTutor + gpa_prev + teacher + Session + Grader + Year",
           no_teacherFE = "GPTBase + GPTTutor + gpa_prev + Session + Grader + Year",
           no_graderFE = "GPTBase + GPTTutor + gpa_prev + teacher + Session + Year",
           gpa_session_year = "GPTBase + GPTTutor + gpa_prev + Session + Year",
           unadjusted = "GPTBase + GPTTutor")
fe <- do.call(rbind, lapply(names(specs), function(s)
  cbind(spec = s, n = nobs(lm(f_main("Part3Tot", specs[[s]]), data = df)), rbind(cr1("Part3Tot", "T-B", specs[[s]]), cr1("Part3Tot", "GPTBase", specs[[s]])))))
print(fe, digits = 3); write.csv(fe, file.path(OUT, "P3_fe_sensitivity.csv"), row.names = FALSE)

# 5. Design check: teachers by arm (classes)
cls <- df[!duplicated(df$Class), ]
print(table(cls$teacher, cls$`Treatment arm`))


# 6. Software information
print(sessionInfo())
writeLines(capture.output(sessionInfo()), file.path(OUT, "P3_sessionInfo.txt"))
