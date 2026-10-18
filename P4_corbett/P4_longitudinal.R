# =====================================================================
# P4 REANALYSIS - Corbett & Tangen (2026), Computers in Human Behavior 175
# Residual Human States: temporal persistence of residual contrasts
#
# Data: OSF component "Data" of https://osf.io/wseq3 (csvs.zip, 9 Qualtrics
# exports). Path via CORBETT_CSV_DIR (see README).
# Packages: dplyr, tidyr, nlme, emmeans, afex, lme4, lmerTest, mice, car, geepack
# Outputs in output/P4/ (or $OUT_DIR); the console log reproduces all reported numbers.
# =====================================================================
CSV_DIR <- Sys.getenv("CORBETT_CSV_DIR", "data/corbett_tangen/csvs")
OUT <- Sys.getenv("OUT_DIR", "output/P4"); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ---------------- PART A: build participant-level dataset ------------
# Strongest misconception = item with max pre rating (first max, as in the
# authors' which.max); participants with any missing post item dropped
# (authors' rule). Result: 125 per arm; 311 complete cases in the
# published benchmark, 312 after recovering one 2-month ID linkage.
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
rd <- function(f) { d <- read.csv(f, check.names = TRUE); list(q = d[1,], d = d[-(1:2),]) }
item_text <- function(q, cols) sub(".* - ", "", unlist(q[1, cols]))
get_block <- function(obj, prefix) {
  cols <- grep(paste0("^", prefix, "_[0-9]+$"), names(obj$d), value = TRUE)
  suf <- sub(".*_", "", cols)
  out <- obj$d[, c("PROLIFIC_PID", cols)]
  names(out) <- c("id", paste0("i", suf))
  out[-1] <- lapply(out[-1], as.numeric)
  attr(out, "text") <- setNames(item_text(obj$q, cols), paste0("i", suf))
  out
}
conds <- c(misconception_ai = "M", textbook_reading = "T", neutral_ai = "N")
all <- list(); texts <- list()
for (cn in names(conds)) {
  t1 <- rd(sprintf(paste0(CSV_DIR, "/%s_time1.csv"), cn))
  t2 <- rd(sprintf(paste0(CSV_DIR, "/%s_time2.csv"), cn))
  t3 <- rd(sprintf(paste0(CSV_DIR, "/%s_time3.csv"), cn))
  prep <- if (any(grepl("^pre.test\\._", names(t1$d)))) "pre.test\\." else "pre.test"
  postp <- if (any(grepl("^post.test\\._", names(t1$d)))) "post.test\\." else "post.test"
  pre <- get_block(t1, prep); post <- get_block(t1, postp)
  f10 <- get_block(t2, "followup_10days"); f2m <- get_block(t3, "followup_2months")
  texts[[cn]] <- list(pre = attr(pre,"text"), post = attr(post,"text"),
                      f10 = attr(f10,"text"), f2m = attr(f2m,"text"))
  # authors' rule: drop participants with any NA in post-test items
  na_ids <- post$id[apply(is.na(post[-1]), 1, any)]
  pre <- filter(pre, !id %in% na_ids); post <- filter(post, !id %in% na_ids)
  items <- names(pre)[-1]
  strongest <- items[apply(as.matrix(pre[items]), 1, which.max)]
  key <- tibble(id = pre$id, item = strongest,
                pre = as.matrix(pre[items])[cbind(seq_len(nrow(pre)), match(strongest, items))])
  pick <- function(blk) { m <- match(key$id, blk$id)
    v <- as.matrix(blk[items])[cbind(m, match(key$item, items))]; v }
  key$post <- pick(post); key$d10 <- pick(f10); key$m2 <- pick(f2m)
  key$cond <- conds[[cn]]
  # follow-up files: one response per participant; flag responses linked through the
  # self-reported Prolific ID by tools/sanitize_corbett.py (PID_source == "Q48")
  for (tt in list(t2, t3)) { ids <- tt$d$PROLIFIC_PID[tt$d$PROLIFIC_PID != ""]; stopifnot(!anyDuplicated(ids)) }
  stopifnot("PID_source" %in% names(t3$d))
  key$m2_from_q48 <- t3$d$PID_source[match(key$id, t3$d$PROLIFIC_PID)] %in% "Q48" & !is.na(key$m2)
  # check: recomputed strongest item equals the item Qualtrics used in the intervention
  rec <- trimws(t1$d$StrongestMisconception[match(key$id, t1$d$PROLIFIC_PID)])
  stopifnot(all(trimws(texts[[cn]]$pre[key$item]) == rec))
  cat(cn, ": strongest item matches recorded StrongestMisconception for", length(rec), "participants\n")
  key$n_items_pre_na <- rowSums(is.na(pre[items]))
  all[[cn]] <- key
}
wide <- bind_rows(all) %>% mutate(cond = factor(cond, levels = c("N","T","M")))
# check wording consistency of items across blocks
for (cn in names(texts)) { tx <- texts[[cn]]
  ok <- sapply(c("post","f10","f2m"), function(b) all(tx$pre[names(tx$pre)] == tx[[b]][names(tx$pre)]))
  cat(cn, "item wording identical across blocks:", ok, "\n"); stopifnot(all(ok)) }
cat("two-month responses linked through the self-reported ID:", sum(wide$m2_from_q48), "\n")
# frozen linkage: exactly one linked response (Neutral AI, two months); resulting sample sizes
stopifnot(sum(wide$m2_from_q48) == 1, as.character(wide$cond[wide$m2_from_q48]) == "N",
          sum(!is.na(wide$m2)) == 325, sum(!is.na(wide$d10)) == 356)
cat("\nN by condition:\n"); print(table(wide$cond))
cat("max pre > 50:\n"); print(table(wide$cond, wide$pre > 50))
wide <- wide %>% mutate(is_cc = !is.na(pre) & !is.na(post) & !is.na(d10) & !is.na(m2))
print(wide %>% group_by(cond) %>% summarise(n = n(), n_post = sum(!is.na(post)),
      n_10d = sum(!is.na(d10)), n_2m = sum(!is.na(m2)), n_complete = sum(is_cc)))
wide <- wide %>% select(-is_cc)
# Derived participant-level file (pseudonymised); written for local checks only, not distributed
write.csv(wide %>% mutate(id = sprintf("P%03d", match(id, unique(id)))), file.path(OUT, "wide_strongest.csv"), row.names = FALSE)

# ---------------- PART B: analyses ---------------------------------
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(nlme); library(emmeans)
  library(afex); library(lme4); library(lmerTest); library(mice)})
emm_options(msg.interaction = FALSE)
set.seed(20260928)
w <- wide %>% mutate(id = factor(id))

cat("=== 1. REPRODUCTION: immediate ANCOVA (N=375) ===\n")
m_anc <- lm(post ~ pre + cond, data = w)
a <- anova(m_anc); print(a)
cat("partial eta2 cond:", a["cond","Sum Sq"]/(a["cond","Sum Sq"]+a["Residuals","Sum Sq"]), "\n")
print(car::Anova(m_anc, type = 3))
# Published values: F(2,371) = 46.59 equals the omnibus F of the whole model (3, 371 df);
# eta_p^2 = .230 equals condition entered before the covariate (sequential SS, unadjusted)
fs <- summary(m_anc)$fstatistic
cat("omnibus model F(", fs[2], ",", fs[3], ") =", fs[1], "\n")
a_seq <- anova(lm(post ~ cond + pre, data = w))
cat("partial eta2 cond entered first (sequential SS):",
    a_seq["cond","Sum Sq"]/(a_seq["cond","Sum Sq"]+a_seq["Residuals","Sum Sq"]), "\n")
print(summary(pairs(emmeans(m_anc, ~cond), adjust="tukey"), infer=TRUE))

cat("\n=== 2. REPRODUCTION: RM-ANCOVA complete cases ===\n")
# The published benchmark used the embedded IDs only, so the response linked through the
# self-reported ID is set aside here (published N = 311); all later analyses include it.
cc <- w %>% mutate(m2 = ifelse(m2_from_q48, NA, m2)) %>%
  filter(complete.cases(pre, post, d10, m2)) %>% mutate(pre_c = pre - mean(pre))
stopifnot(nrow(cc) == 311)
cc_long <- cc %>% pivot_longer(c(post,d10,m2), names_to="time", values_to="belief") %>%
  mutate(time = factor(time, levels=c("post","d10","m2")))
m_rm <- aov_ez("id","belief", cc_long, between="cond", within="time", covariate="pre_c",
               factorize=FALSE, anova_table=list(correction="none", es="pes"))
print(nrow(cc)); print(m_rm)
rm_pairs <- summary(pairs(emmeans(m_rm, ~cond|time), adjust="tukey"), infer=TRUE); print(rm_pairs)
rm_pairs_unadj <- summary(pairs(emmeans(m_rm, ~cond|time), adjust="none"), infer=TRUE)
print(rm_pairs_unadj[rm_pairs_unadj$contrast=="T - M",])
# Published "main effect of Time" (F(2,614) = 0.28, p = .756): authors' specification is
# car::Anova(type = "III") on a multivariate lm with treatment contrasts (reference level
# Misconception AI) and the uncentred covariate, i.e. the simple effect of Time in the
# Misconception AI arm at pre = 0, not an average over conditions.
op <- options(contrasts = c("contr.treatment", "contr.poly"))
m_pub <- lm(cbind(post, d10, m2) ~ pre + relevel(factor(as.character(cond)), "M"), data = cc)
a_pub <- summary(car::Anova(m_pub, idata = data.frame(Time = factor(1:3)), idesign = ~Time, type = "III"),
                 multivariate = FALSE)$univariate.tests
cat("published-specification Time effect: F =", a_pub["Time", "F value"], " p =", a_pub["Time", "Pr(>F)"], "\n")
options(op)

# Complete cases for the reanalysis (includes the linked two-month response)
cc <- w %>% filter(complete.cases(pre, post, d10, m2)) %>% mutate(pre_c = pre - mean(pre))
cat("complete cases (reanalysis):", nrow(cc), "\n")

cat("\n=== 3. ATTRITION ===\n")
w <- w %>% mutate(R10=!is.na(d10), R2m=!is.na(m2))
print(table(w$cond, w$R2m)); print(chisq.test(table(w$cond, w$R10))); print(chisq.test(table(w$cond, w$R2m)))
print(summary(glm(R2m ~ cond + pre + post, family=binomial, data=w))$coefficients)

cat("\n=== 4. MMRM (gls, unstructured) ===\n")
long <- w %>% mutate(pre_c = pre - mean(pre)) %>%
  pivot_longer(c(post,d10,m2), names_to="time", values_to="belief") %>%
  mutate(time = factor(time, levels=c("post","d10","m2")), tnum = as.integer(time)) %>%
  arrange(id, time) %>% filter(!is.na(belief))
m_mmrm <- gls(belief ~ cond*time + pre_c*time, data=long,
              correlation = corSymm(form = ~ tnum | id),
              weights = varIdent(form = ~ 1 | time), method="REML")
print(anova(m_mmrm, type="marginal"))
print(getVarCov(m_mmrm, individual = levels(long$id)[1]))
cat("N participants:", n_distinct(long$id), " obs:", nrow(long), "\n")
stopifnot(n_distinct(long$id) == 375, nrow(long) == 1056)
cells <- emmeans(m_mmrm, ~ cond*time, mode = "satterthwaite")
print(joint_tests(cells))
emm <- emmeans(m_mmrm, ~ cond | time, mode = "satterthwaite")
D <- contrast(emm, method=list("M-N"=c(-1,0,1), "M-T"=c(0,-1,1)), adjust="none")
Dt <- as.data.frame(summary(D, infer=TRUE)); Dt$p_holm <- p.adjust(Dt$p.value,"holm"); print(Dt)

g <- as.data.frame(cells@grid)
wt <- function(c_,t_) as.numeric(g$cond==c_ & g$time==t_)
DD <- function(j,t1,t2) (wt("M",t2)-wt(j,t2)) - (wt("M",t1)-wt(j,t1))
dD <- contrast(cells, adjust="none", method=list(
  "dD_M-N (2m-post)"=DD("N","post","m2"), "dD_M-T (2m-post)"=DD("T","post","m2"),
  "dD_M-N (10d-post)"=DD("N","post","d10"), "dD_M-T (10d-post)"=DD("T","post","d10"),
  "dD_M-N (2m-10d)"=DD("N","d10","m2"), "dD_M-T (2m-10d)"=DD("T","d10","m2")))
dDt <- as.data.frame(summary(dD, infer=TRUE))
prim <- grepl("2m-post", dDt$contrast); dDt$p_holm_primary <- NA
dDt$p_holm_primary[prim] <- p.adjust(dDt$p.value[prim], "holm"); print(dDt)
within <- as.data.frame(summary(contrast(cells, adjust="none", method=list(
  "M 2m-post"=wt("M","m2")-wt("M","post"), "T 2m-post"=wt("T","m2")-wt("T","post"),
  "N 2m-post"=wt("N","m2")-wt("N","post"))), infer=TRUE)); print(within)
tost <- function(lbl, margin) { x <- as.data.frame(summary(dD, infer=TRUE, level=.90))
  x <- x[x$contrast==lbl,]
  p <- max(pt((x$estimate+margin)/x$SE, x$df, lower.tail=FALSE), pt((x$estimate-margin)/x$SE, x$df))
  data.frame(contrast=lbl, margin, est=x$estimate, lo90=x$lower.CL, hi90=x$upper.CL, p_TOST=p) }
print(do.call(rbind, lapply(c(5,7.5,10), function(m) tost("dD_M-T (2m-post)", m))))

cat("\n=== 5. ROBUSTNESS ===\n")
m_lmm <- lmer(belief ~ cond*time + pre_c + (1|id), data=long)
Dl <- as.data.frame(summary(contrast(emmeans(m_lmm, ~cond|time), method=list("M-N"=c(-1,0,1),"M-T"=c(0,-1,1))), infer=TRUE))
dDl <- as.data.frame(summary(contrast(emmeans(m_lmm, ~cond*time), adjust="none", method=list(
  "dD_M-N (2m-post)"=DD("N","post","m2"), "dD_M-T (2m-post)"=DD("T","post","m2"))), infer=TRUE))
anc <- do.call(rbind, lapply(c("post","d10","m2"), function(h) {
  m <- lm(as.formula(paste(h,"~ cond + pre")), data=w)
  x <- as.data.frame(summary(contrast(emmeans(m,~cond), method=list("M-N"=c(-1,0,1),"M-T"=c(0,-1,1))), infer=TRUE))
  x$time <- h; x$n <- nobs(m); x }))
print(Dl); print(dDl); print(anc)
# complete-case difference-score model for Delta D
ccd <- cc %>% mutate(chg = m2 - post)
ccs <- as.data.frame(summary(contrast(emmeans(lm(chg ~ cond + pre, data=ccd), ~cond),
        method=list("dD_M-N (2m-post)"=c(-1,0,1),"dD_M-T (2m-post)"=c(0,-1,1))), infer=TRUE))
print(ccs)
# temporal contrasts under the alternative specifications (LMM; complete-case change score)
keep <- c("contrast","estimate","SE","df","lower.CL","upper.CL","p.value")
write.csv(rbind(transform(dDl[, keep], model="LMM"), transform(ccs[, keep], model=paste0("Complete-case change score (N=", nrow(ccd), ")"))),
          file.path(OUT, "P4_robustness_temporal.csv"), row.names=FALSE)

write.csv(Dt, file.path(OUT, "P4_horizon_contrasts_MMRM.csv"), row.names=FALSE)
write.csv(dDt, file.path(OUT, "P4_temporal_contrasts_MMRM.csv"), row.names=FALSE)
write.csv(within, file.path(OUT, "P4_within_arm_change.csv"), row.names=FALSE)
write.csv(rbind(transform(Dl, model="LMM"), transform(anc[,names(Dl)], model="ANCOVA")), file.path(OUT, "P4_robustness.csv"), row.names=FALSE)

# ---------------- PART C: MNAR sensitivity --------------------------
cat("\n=== 6. TIPPING POINT (delta-adjusted MI) ===\n")
dat <- wide %>% select(cond, pre, post, d10, m2)
ini <- mice(dat, maxit=0, printFlag=FALSE); meth <- ini$method; meth[c("d10","m2")] <- "norm"
imp <- mice(dat, m=40, method=meth, printFlag=FALSE, seed=20260928)
base <- complete(imp, "long", include=TRUE)
miss0 <- rep(is.na(dat$m2), imp$m + 1) & base$.imp > 0
tip <- function(delta, arm) {
  comp <- base; sel <- miss0 & comp$cond == arm
  comp$m2[sel] <- comp$m2[sel] + delta
  fit <- with(as.mids(comp), lm(I(m2 - post) ~ relevel(cond, "M") + pre))
  s <- summary(pool(fit), conf.int=TRUE); s <- s[grepl("relevel", s$term),]
  data.frame(arm, delta, contrast = ifelse(grepl("N$", s$term), "dD_M-N", "dD_M-T"),
             est = -s$estimate, lo = -s$`97.5 %`, hi = -s$`2.5 %`, p = s$p.value)
}
grid <- expand.grid(arm=c("M","N","T"), delta=seq(-30,30,5), stringsAsFactors=FALSE)
tp <- do.call(rbind, Map(tip, grid$delta, grid$arm))
tp$sig <- tp$lo > 0 | tp$hi < 0
write.csv(tp, file.path(OUT, "P4_tipping_point.csv"), row.names=FALSE)
print(tp[tp$delta==0,])
cat("\nM-N: deltas where CI includes 0\n"); print(tp[tp$contrast=="dD_M-N" & !tp$sig, c("arm","delta","est","lo","hi")])
cat("\nM-T: deltas where CI excludes 0\n"); print(tp[tp$contrast=="dD_M-T" & tp$sig, c("arm","delta","est","lo","hi")])


# Range diagnostic: "norm" imputation is unbounded on the 0-100 belief scale
rng <- do.call(rbind, lapply(c("d10", "m2"), function(v) { x <- unlist(imp$imp[[v]])
  data.frame(variable = v, n_imputed = length(x), below_0 = sum(x < 0), above_100 = sum(x > 100),
             share_outside = mean(x < 0 | x > 100), min = min(x), max = max(x)) }))
print(rng, digits = 3); write.csv(rng, file.path(OUT, "P4_imputation_range.csv"), row.names = FALSE)

# Bounded sensitivity: (a) norm imputations clamped to [0,100] after the delta shift;
# (b) predictive mean matching (imputes observed values only), also clamped after the
# delta shift; same delta grid and seed
tip_variant <- function(base_v, miss_v, clamp) function(delta, arm) {
  comp <- base_v; sel <- miss_v & comp$cond == arm; comp$m2[sel] <- comp$m2[sel] + delta
  if (clamp) { imp_rows <- comp$.imp > 0
    comp$m2[miss_v] <- pmin(100, pmax(0, comp$m2[miss_v]))
    comp$d10[imp_rows] <- pmin(100, pmax(0, comp$d10[imp_rows])) }
  fit <- with(as.mids(comp), lm(I(m2 - post) ~ relevel(cond, "M") + pre))
  s <- summary(pool(fit), conf.int = TRUE); s <- s[grepl("relevel", s$term), ]
  data.frame(arm, delta, contrast = ifelse(grepl("N$", s$term), "dD_M-N", "dD_M-T"),
             est = -s$estimate, lo = -s$`97.5 %`, hi = -s$`2.5 %`, p = s$p.value)
}
meth_pmm <- meth; meth_pmm[c("d10", "m2")] <- "pmm"
imp_pmm <- mice(dat, m = 40, method = meth_pmm, printFlag = FALSE, seed = 20260928)
base_pmm <- complete(imp_pmm, "long", include = TRUE)
bounded <- rbind(
  cbind(variant = "norm, clamped to [0,100]", do.call(rbind, Map(tip_variant(base, miss0, TRUE), grid$delta, grid$arm))),
  cbind(variant = "pmm, clamped to [0,100]", do.call(rbind, Map(tip_variant(base_pmm, miss0, TRUE), grid$delta, grid$arm))))
bounded$sig <- bounded$lo > 0 | bounded$hi < 0
write.csv(bounded, file.path(OUT, "P4_tipping_point_bounded.csv"), row.names = FALSE)

# Arm-level decomposition of dD_M-N when only the Neutral arm departs from MAR
# (share of the attenuation corresponding to movement of the Neutral arm), for the
# unbounded primary imputation and the two bounded variants
mp <- mean(dat$pre)
decomp <- function(base_v, clamp, label) do.call(rbind, lapply(seq(0, 30, 5), function(d) {
  comp <- base_v; sel <- miss0 & comp$cond == "N"; comp$m2[sel] <- comp$m2[sel] + d
  if (clamp) { r <- comp$.imp > 0; comp$m2[miss0] <- pmin(100, pmax(0, comp$m2[miss0]))
    comp$d10[r] <- pmin(100, pmax(0, comp$d10[r])) }
  e <- rowMeans(sapply(1:imp$m, function(i) { x <- comp[comp$.imp == i, ]
    coef(lm(I(m2 - post) ~ 0 + cond + I(pre - mp), data = x))[c("condM", "condN")] }))
  data.frame(variant = label, delta_N = d, change_M = e[["condM"]], change_N = e[["condN"]],
             dD_MN = e[["condM"]] - e[["condN"]], share_N = -e[["condN"]] / (e[["condM"]] - e[["condN"]]))
}))
dec <- rbind(decomp(base, FALSE, "norm (primary, unbounded)"),
             decomp(base, TRUE, "norm, clamped to [0,100]"),
             decomp(base_pmm, TRUE, "pmm, clamped to [0,100]"))
print(dec, digits = 3); write.csv(dec, file.path(OUT, "P4_decomposition_MNAR.csv"), row.names = FALSE)
print(do.call(rbind, lapply(split(bounded, list(bounded$variant, bounded$contrast)), function(x)
  data.frame(variant = x$variant[1], contrast = x$contrast[1], est_min = min(x$est), est_max = max(x$est),
             n_scenarios = nrow(x), n_excluding_0 = sum(x$sig)))), digits = 3)

# ---------------- PART D: attrition table, GEE and IPW-GEE -------------
suppressPackageStartupMessages(library(geepack))
w <- wide %>% mutate(id = factor(id))
# attrition table
att <- w %>% group_by(cond) %>% summarise(n_pre=sum(!is.na(pre)), n_post=sum(!is.na(post)),
  n_10d=sum(!is.na(d10)), n_2m=sum(!is.na(m2)), n_complete=sum(complete.cases(pre,post,d10,m2)),
  ret_10d=mean(!is.na(d10)), ret_2m=mean(!is.na(m2)))
print(att); write.csv(att, file.path(OUT, "P4_attrition_by_condition.csv"), row.names=FALSE)
print(chisq.test(table(w$cond, !is.na(w$d10)))); 
long <- w %>% mutate(pre_c=pre-mean(pre)) %>% pivot_longer(c(post,d10,m2), names_to="time", values_to="belief") %>%
  mutate(time=factor(time, levels=c("post","d10","m2")), wave=as.integer(time)) %>% filter(!is.na(belief)) %>% arrange(id, wave)
# IPW: P(observed at wave | cond, pre, post) ; post always observed -> weight 1
p10 <- glm(!is.na(d10) ~ cond + pre + post, binomial, data=w); p2m <- glm(!is.na(m2) ~ cond + pre + post, binomial, data=w)
w$ipw10 <- 1/predict(p10, type="response"); w$ipw2m <- 1/predict(p2m, type="response")
long <- long %>% left_join(w %>% select(id, ipw10, ipw2m), by="id") %>%
  mutate(ipw = case_when(time=="post" ~ 1, time=="d10" ~ ipw10, TRUE ~ ipw2m))
g <- function(weights=NULL, cs="exchangeable") {
  if (is.null(weights)) geeglm(belief ~ cond*time + pre_c*time, id=id, waves=wave, data=long, corstr=cs)
  else geeglm(belief ~ cond*time + pre_c*time, id=id, waves=wave, data=long, corstr=cs, weights=ipw) }
res <- list()
for (nm in c("GEE exchangeable","IPW-GEE independence")) {
  m <- switch(nm, "GEE exchangeable"=g(), "IPW-GEE independence"=g(TRUE,"independence"))
  cells <- emmeans(m, ~cond*time); gr <- as.data.frame(cells@grid)
  wt <- function(c_,t_) as.numeric(gr$cond==c_ & gr$time==t_)
  DD <- function(j,t1,t2) (wt("M",t2)-wt(j,t2))-(wt("M",t1)-wt(j,t1))
  meth <- list(); for (t_ in c("post","d10","m2")) { meth[[paste0("D_M-N ",t_)]] <- wt("M",t_)-wt("N",t_); meth[[paste0("D_M-T ",t_)]] <- wt("M",t_)-wt("T",t_) }
  meth[["dD_M-N (2m-post)"]] <- DD("N","post","m2"); meth[["dD_M-T (2m-post)"]] <- DD("T","post","m2")
  x <- as.data.frame(summary(contrast(cells, method=meth, adjust="none"), infer=TRUE)); x$model <- nm; res[[nm]] <- x
}
r <- bind_rows(res)[,c("model","contrast","estimate","SE","asymp.LCL","asymp.UCL","p.value")]
print(r, digits=3); write.csv(r, file.path(OUT, "P4_robustness_GEE_IPW.csv"), row.names=FALSE)



# ---------------- PART E: retention model (manuscript appendix table) -----
w2 <- wide %>% mutate(R2m = !is.na(m2), pre10 = pre / 10, post10 = post / 10)
mret <- glm(R2m ~ cond + pre10 + post10, family = binomial, data = w2)
cf <- summary(mret)$coefficients; ci <- confint.default(mret)
ret <- data.frame(term = rownames(cf), OR = exp(cf[, 1]), lo = exp(ci[, 1]), hi = exp(ci[, 2]), p = cf[, 4])
print(ret, digits = 3); write.csv(ret, file.path(OUT, "P4_retention_model.csv"), row.names = FALSE)
v2m <- tapply(cc$m2, cc$cond, var); n2m <- table(cc$cond)
cat("pooled within-group SD of two-month belief (complete cases, df-weighted):",
    sqrt(sum((n2m - 1) * v2m) / sum(n2m - 1)), "\n")

# ---------------- session information ------------------------------
print(sessionInfo())
