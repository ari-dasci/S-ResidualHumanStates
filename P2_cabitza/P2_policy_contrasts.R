# =====================================================================
# P2 REANALYSIS - Cabitza & Vicente (2026), Int. J. Human-Computer Studies 215, 103855
# Residual Human States: interaction-policy dependence of later unaided accuracy
#
# Data: authors' open data folder (Google Drive, see README), file
#   MachineMentoring_DataHD.csv (semicolon-separated, Latin-1; one row per participant;
#   responses coded 1 = correct, 0 = incorrect). Path via CABITZA_DIR.
# Packages: readr, sandwich
# Outputs in output/P2/ (or $OUT_DIR); the console log reproduces all reported numbers.
# =====================================================================
suppressPackageStartupMessages({library(readr); library(sandwich)})
CABITZA_DIR <- Sys.getenv("CABITZA_DIR", "data/cabitza_vicente/Data")
OUT <- Sys.getenv("OUT_DIR", "output/P2"); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

raw <- read_delim(file.path(CABITZA_DIR, "MachineMentoring_DataHD.csv"), delim = ";",
                  locale = locale(encoding = "latin1"), show_col_types = FALSE)
stopifnot(nrow(problems(raw)) == 0)                      # parsing check on the tibble itself
d <- as.data.frame(raw); stopifnot(nrow(d) == 289)
print(table(d$Group))                                    # Advice-only 70, Confidence 94, Control 68, Feedback 57

# Case sets as documented in the authors' data notebook
hidden <- paste0("H", c(126,131,135,138,141,145,146,148,149,150,153,154,157,158,161,163,164,167,169,
                        172,173,175,177,180,181,185,186,187,190,191))   # Phase 2, hidden criterion (30)
phase2 <- paste0("H", 123:192)                                          # Phase 2, all cases (70)
known  <- setdiff(phase2, hidden)                                       # Phase 2, known criteria (40)
test   <- paste0("H", 13:22)                                            # pre-intervention test phase (10)
stopifnot(length(hidden) == 30, length(known) == 40,
          all(unlist(d[, c(phase2, test)]) %in% c(0, 1)))               # complete 0/1 responses

# Participant-level outcomes; analysis restricted to the three AI-supported policies
a <- d[d$Group != "Control", ]
a$policy <- factor(a$Group, levels = c("Feedback", "Advice-only", "Confidence"),
                   labels = c("Feedback", "Advice", "Advice+Confidence"))
a$O_H <- rowMeans(a[, hidden]); a$known <- rowMeans(a[, known]); a$all <- rowMeans(a[, phase2])
a$baseline <- rowMeans(a[, test])
stopifnot(nrow(a) == 221)

# 1. Descriptives (Table 2 of the original article reports these values truncated to 3 decimals)
desc <- do.call(rbind, lapply(split(a, a$policy), function(x) data.frame(
  policy = x$policy[1], n = nrow(x), mean_hidden = mean(x$O_H), sd_hidden = sd(x$O_H),
  mean_known = mean(x$known), mean_all = mean(x$all), mean_baseline = mean(x$baseline))))
print(desc, digits = 4); write.csv(desc, file.path(OUT, "P2_descriptives.csv"), row.names = FALSE)

# 2. Focal contrasts: two-sided Welch tests, Hedges' g, Holm across the two contrasts
hedges_g <- function(x, y) { n1 <- length(x); n2 <- length(y)
  sp <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  (mean(x) - mean(y)) / sp * (1 - 3 / (4 * (n1 + n2) - 9)) }
fb <- a$O_H[a$policy == "Feedback"]
welch <- do.call(rbind, lapply(c("Advice", "Advice+Confidence"), function(o) {
  y <- a$O_H[a$policy == o]; t <- t.test(fb, y)
  data.frame(contrast = paste0("Feedback - ", o), estimate = mean(fb) - mean(y), se = t$stderr,
             lo = t$conf.int[1], hi = t$conf.int[2], df = unname(t$parameter),
             t = unname(t$statistic), p = t$p.value, hedges_g = hedges_g(fb, y)) }))
welch$p_holm <- p.adjust(welch$p, "holm")
print(welch, digits = 4); write.csv(welch, file.path(OUT, "P2_welch_contrasts.csv"), row.names = FALSE)

# 3. Sensitivity: OLS adjusted for pre-intervention test-phase accuracy, HC3 SEs, normal (Wald) reference
m <- lm(O_H ~ policy + baseline, data = a); V <- vcovHC(m, type = "HC3")
adj <- do.call(rbind, lapply(c("Advice", "Advice+Confidence"), function(o) {
  k <- paste0("policy", o); e <- -coef(m)[[k]]; s <- sqrt(V[k, k])
  data.frame(contrast = paste0("Feedback - ", o), estimate = e, se = s,
             lo = e - qnorm(.975) * s, hi = e + qnorm(.975) * s, p = 2 * pnorm(-abs(e / s)), n = nobs(m)) }))
print(adj, digits = 4); write.csv(adj, file.path(OUT, "P2_hc3_adjusted.csv"), row.names = FALSE)

# ---------------- session information ------------------------------
print(sessionInfo())
