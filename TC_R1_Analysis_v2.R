###############################################################################
# Teleconsultation uptake across 24 countries: revision (R1, R2) analyses
# Sousi S, et al.
#
# Reproduces the original descriptive and association analyses and adds the
# analyses requested by reviewers:
#   1. Correct test for binary predictors vs continuous outcome (Mann-Whitney U,
#      Hodges-Lehmann difference with 95% CI)                     [R3, R6]
#   2. Effect sizes with 95% CIs (Spearman rho, epsilon-squared, Cramer's V)
#      and Holm-adjusted p-values for multiplicity                 [R4, R5, R6]
#   3. Sensitivity analyses: alternative uptake cut-offs, exclusion of Greece
#      and Romania, restriction to countries with comparable definitions [R4-R7]
#   4. Longitudinal outcomes: change from 2021 to the latest year,
#      retention ratio, AAPC                                       [R4, R6]
#   5. Joinpoint regression (joinpointR) with APC/AAPC and 95% CIs  [R4-R7]
#   6. Population insurance coverage as an additional characteristic [R3]
#   7. Revised Figure 1 (darker lines) and joinpoint panel figure  [R3]
#
# Input : Tele_Consultations_Data.xlsx (sheets "Table1", "TC prop",
#         "Country Chars")
# Output: ./output/*.csv, ./output/TC_R1_results.xlsx, ./output/*.png
#
# Packages: readxl, dplyr, tidyr, ggplot2, boot, openxlsx, joinpointR (>= 1.1.0),
#           segmented (>= 2.1-0)
###############################################################################

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(ggplot2); library(boot); library(openxlsx)
})
set.seed(20260926)
f   <- "Tele Consultations Data.xlsx"
out <- "output"; dir.create(out, showWarnings = FALSE)

# ----------------------------------------------------------------------------
# 1. Data
# ----------------------------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(x))

# 1a. Annual time series (consultations per person, in-person and tele)
ts <- read_excel(f, "Table1") %>%
  rename(year = Year, f2f = F2F, tc = TC) %>%
  mutate(f2f = num(f2f), tc = num(tc),
         prop = 100 * tc / (f2f + tc)) %>%          # % of physician consultations
  filter(!is.na(Country))

# 1b. Country-level file (outcome and digital health indicators)
tcp <- read_excel(f, "TC prop") %>%
  filter(!is.na(Country), Country != "England") %>%   # England not in OECD data
  transmute(Country,
            latest_year = `Latest Year`,
            p2019   = 100 * num(TC_prop_2019),
            p2021   = 100 * num(TC_prop_2021),
            p_latest = 100 * num(TC_prop_latest),
            internet = 100 * num(`Internet Coverage`),
            dh_policy   = num(`National digital health policy or strategy`),
            th_policy   = num(`National telehealth policy or strategy`),
            dh_edu      = num(`Digital health education action plan, policy or strategy`),
            inservice   = num(`Health professionals: in-service training in digital health (2022)`),
            certified   = num(`Health science students: certified training in digital health (2022)`))

cc <- read_excel(f, "Country Chars")
nm <- names(cc)
pick <- function(pattern) nm[grepl(pattern, nm, fixed = TRUE)][1]
chars <- cc %>% transmute(
  Country,
  gdp       = num(.data[[pick("GDP (US$)")]]),
  gni       = num(.data[[pick("Gross National Income")]]),
  urban     = num(.data[[pick("Urbanicity")]]),
  literacy  = num(.data[[pick("Literacy rate")]]),
  upper_sec = num(.data[[pick("Upper secondary")]]),
  tertiary  = num(.data[[pick("Tertiary Education")]]),
  the       = num(.data[[pick("Total health expenditure")]]),
  rd        = num(.data[[pick("Research & Development expenditure")]]),
  phys      = num(.data[[pick("Number of practicing physicians")]]),
  nurses    = num(.data[[pick("Nurses and midwives")]]),
  insurance = num(.data[[pick("compulsory health insurance")]]),
  fund_public  = num(.data[[pick("Public funding (2022)")]]),
  fund_private = num(.data[[pick("Private or commercial funding")]]),
  fund_donor   = num(.data[[pick("Donor/non-public funding")]]),
  fund_ppp     = num(.data[[pick("Public private partnerships")]]))
chars$insurance <- ifelse(chars$insurance <= 1, 100 * chars$insurance, chars$insurance)

non_who_eu <- c("Australia", "Canada", "Chile", "Korea")
d <- tcp %>% left_join(chars, by = "Country") %>%
  mutate(
    # WHO Europe survey variables: unknown for the four non-European countries.
    # Within WHO Europe, a blank funding response is treated as "No" (as in the
    # original analysis).
    across(starts_with("fund_"), ~ ifelse(Country %in% non_who_eu, NA, coalesce(.x, 0))),
    uptake_cat = cut(p_latest, c(-Inf, 5, 20.0000001, Inf), right = FALSE,
                     labels = c("Low (<5%)", "Medium (5-20%)", "High (>20%)")),
    change_pp  = p_latest - p2021,                      # post-pandemic change
    retention  = ifelse(p2021 > 0, p_latest / p2021, NA),
    increase_pp = p2021 - p2019)                        # pandemic increase
stopifnot(nrow(d) == 24)
write.csv(d, file.path(out, "analysis_dataset.csv"), row.names = FALSE)

cont_vars <- c(gdp = "GDP (US$)", gni = "GNI per capita (US$)",
               urban = "Urbanicity (%)", internet = "Internet coverage (%)",
               literacy = "Literacy rate (15+) (%)",
               upper_sec = "Upper secondary education (25+) (%)",
               tertiary = "Tertiary education (25+) (%)",
               the = "Total health expenditure (% GDP)",
               rd = "R&D expenditure (% GDP)",
               phys = "Practising physicians per 1,000",
               nurses = "Nurses and midwives per 1,000",
               insurance = "Population covered by compulsory health insurance (%)")
bin_vars  <- c(fund_public = "Public funding", fund_private = "Private/commercial funding",
               fund_donor = "Donor/non-public funding", fund_ppp = "Public-private partnerships",
               dh_policy = "National digital health policy/strategy",
               th_policy = "National telehealth policy/strategy",
               dh_edu = "Digital health education policy/plan",
               inservice = "In-service digital health training (health professionals)",
               certified = "Certified digital health training (health science students)")

# ----------------------------------------------------------------------------
# 2. Helper functions
# ----------------------------------------------------------------------------
fmt <- function(x, k = 2) formatC(x, format = "f", digits = k)

spearman_row <- function(x, y, B = 2000) {
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  ct <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  bt <- boot(cbind(x, y), function(z, i) cor(z[i, 1], z[i, 2], method = "spearman"), B)
  ci <- quantile(bt$t, c(.025, .975), na.rm = TRUE)
  data.frame(n = length(x), test = "Spearman", estimate = unname(ct$estimate),
             lcl = ci[1], ucl = ci[2], p = ct$p.value,
             summary = paste0("rho = ", fmt(ct$estimate), " (", fmt(ci[1]), " to ", fmt(ci[2]), ")"))
}

mwu_row <- function(g, y) {            # binary predictor vs continuous outcome
  ok <- complete.cases(g, y); g <- g[ok]; y <- y[ok]
  y1 <- y[g == 1]; y0 <- y[g == 0]
  if (length(y1) < 2 || length(y0) < 2)
    return(data.frame(n = length(y), test = "Mann-Whitney U", estimate = NA, lcl = NA,
                      ucl = NA, p = NA, summary = "Not estimable (no variation)"))
  w  <- suppressWarnings(wilcox.test(y1, y0, conf.int = TRUE, exact = FALSE))
  rbis <- 2 * unname(w$statistic) / (length(y1) * length(y0)) - 1   # rank-biserial r
  data.frame(n = length(y), test = "Mann-Whitney U", estimate = unname(w$estimate),
             lcl = w$conf.int[1], ucl = w$conf.int[2], p = w$p.value,
             summary = paste0("Median ", fmt(median(y1), 1), " vs ", fmt(median(y0), 1),
                              "; HL diff ", fmt(w$estimate, 1), " (", fmt(w$conf.int[1], 1),
                              " to ", fmt(w$conf.int[2], 1), "); r = ", fmt(rbis)))
}

kw_row <- function(x, grp, B = 2000) {  # continuous predictor vs uptake category
  ok <- complete.cases(x, grp); x <- x[ok]; grp <- droplevels(grp[ok])
  k  <- kruskal.test(x ~ grp); n <- length(x)
  eps <- function(xx, gg) unname(kruskal.test(xx ~ gg)$statistic) / (length(xx) - 1)
  bt <- boot(data.frame(x, grp), function(z, i) eps(z$x[i], droplevels(z$grp[i])), B,
             strata = grp)
  ci <- quantile(bt$t, c(.025, .975), na.rm = TRUE)
  e  <- eps(x, grp)
  data.frame(n = n, test = "Kruskal-Wallis", estimate = e, lcl = ci[1], ucl = ci[2],
             p = k$p.value,
             summary = paste0("epsilon2 = ", fmt(e), " (", fmt(ci[1]), " to ", fmt(ci[2]), ")"))
}

cramer_v <- function(tab) {
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (min(dim(tab)) < 2) return(NA)
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  sqrt(unname(chi) / (sum(tab) * (min(dim(tab)) - 1)))
}
fisher_row <- function(g, grp, B = 2000) { # binary predictor vs uptake category
  ok <- complete.cases(g, grp); g <- g[ok]; grp <- droplevels(grp[ok])
  tab <- table(g, grp)
  if (nrow(tab) < 2)
    return(data.frame(n = length(g), test = "Fisher exact", estimate = NA, lcl = NA,
                      ucl = NA, p = NA, summary = "Not estimable (no variation)"))
  p   <- fisher.test(tab)$p.value           # Fisher-Freeman-Halton for r x c
  v   <- cramer_v(tab)
  bt  <- boot(data.frame(g, grp), function(z, i) cramer_v(table(z$g[i], z$grp[i])), B)
  ci  <- quantile(bt$t, c(.025, .975), na.rm = TRUE)
  data.frame(n = length(g), test = "Fisher exact", estimate = v, lcl = ci[1], ucl = ci[2],
             p = p, summary = paste0("V = ", fmt(v), " (", fmt(ci[1]), " to ", fmt(ci[2]), ")"))
}

assoc_table <- function(data, outcome, type = c("continuous", "categorical")) {
  type <- match.arg(type)
  rows <- list()
  for (v in names(cont_vars)) {
    r <- if (type == "continuous") spearman_row(data[[v]], data[[outcome]])
    else kw_row(data[[v]], data[[outcome]])
    rows[[v]] <- cbind(variable = cont_vars[[v]], r)
  }
  for (v in names(bin_vars)) {
    r <- if (type == "continuous") mwu_row(data[[v]], data[[outcome]])
    else fisher_row(data[[v]], data[[outcome]])
    rows[[v]] <- cbind(variable = bin_vars[[v]], r)
  }
  res <- bind_rows(rows)
  res$p_holm <- p.adjust(res$p, method = "holm")
  rownames(res) <- NULL
  res
}

# ----------------------------------------------------------------------------
# 3. Descriptives (Table 1, Supplementary country table)
# ----------------------------------------------------------------------------
desc_cont <- sapply(names(cont_vars), function(v)
  paste0(fmt(mean(d[[v]], na.rm = TRUE)), " (", fmt(sd(d[[v]], na.rm = TRUE)), "); n=",
         sum(!is.na(d[[v]]))))
desc_bin  <- sapply(names(bin_vars), function(v)
  paste0(sum(d[[v]] == 1, na.rm = TRUE), "/", sum(!is.na(d[[v]])), " (",
         fmt(100 * mean(d[[v]], na.rm = TRUE), 0), "%)"))
table1 <- data.frame(variable = c(cont_vars, bin_vars), value = c(desc_cont, desc_bin))
write.csv(table1, file.path(out, "Table1_descriptives.csv"), row.names = FALSE)
write.csv(d %>% dplyr::select(Country, latest_year, p2019, p2021, p_latest, uptake_cat,
                              all_of(names(cont_vars)), all_of(names(bin_vars))),
          file.path(out, "SuppTable_country_characteristics.csv"), row.names = FALSE)

# ----------------------------------------------------------------------------
# 4. Main association analyses (revised Tables 3 and 4)
# ----------------------------------------------------------------------------
tab3 <- assoc_table(d, "p_latest", "continuous")
tab4 <- assoc_table(d, "uptake_cat", "categorical")
write.csv(tab3, file.path(out, "Table3_continuous.csv"), row.names = FALSE)
write.csv(tab4, file.path(out, "Table4_categorical.csv"), row.names = FALSE)

# Cross-tab behind the main categorical finding
xt_policy <- with(d, table(dh_policy, uptake_cat, useNA = "no"))
print(xt_policy)
policy_lists <- d %>% filter(!is.na(dh_policy)) %>%
  group_by(uptake_cat, dh_policy) %>% summarise(countries = paste(Country, collapse = ", "), .groups = "drop")
write.csv(policy_lists, file.path(out, "policy_by_uptake_lists.csv"), row.names = FALSE)

# ----------------------------------------------------------------------------
# 5. Sensitivity analyses for the policy association
# ----------------------------------------------------------------------------
policy_test <- function(data, cuts, label) {
  grp <- cut(data$p_latest, c(-Inf, cuts, Inf), right = FALSE)
  ok  <- !is.na(data$dh_policy)
  tab <- table(data$dh_policy[ok], droplevels(grp[ok]))
  mw  <- mwu_row(data$dh_policy, data$p_latest)
  data.frame(analysis = label, n = sum(ok),
             categorical_p = fisher.test(tab)$p.value, cramers_v = cramer_v(tab),
             continuous_p = mw$p, continuous_summary = mw$summary)
}
comparable_excl <- c("Chile", "Greece", "Norway",   # GP only and/or regional coverage
                     "Denmark",                     # includes nurse consultations
                     "Sweden", "Israel",            # asynchronous chat / prescription renewals
                     "Poland")                      # definition not specified
tert <- quantile(d$p_latest, c(1/3, 2/3))
sens <- bind_rows(
  policy_test(d, c(5, 20.0000001),  "Main: <5 / 5-20 / >20%"),
  policy_test(d, c(10, 20.0000001), "Cut-offs <10 / 10-20 / >20%"),
  policy_test(d, c(5, 15),          "Cut-offs <5 / 5-15 / >15%"),
  policy_test(d, c(5, 25),          "Cut-offs <5 / 5-25 / >25%"),
  policy_test(d, tert,              "Tertiles"),
  policy_test(d, median(d$p_latest),"Median split"),
  policy_test(filter(d, !Country %in% c("Greece", "Romania")), c(5, 20.0000001),
              "Excluding Greece and Romania"),
  policy_test(filter(d, !Country %in% comparable_excl), c(5, 20.0000001),
              "Restricted to comparable definitions"))
write.csv(sens, file.path(out, "SuppTable_sensitivity_policy.csv"), row.names = FALSE)
print(sens)

# Full association tables for sensitivity populations (continuous outcome)
tab3_excl_GR_RO <- assoc_table(filter(d, !Country %in% c("Greece", "Romania")), "p_latest")
tab3_comparable <- assoc_table(filter(d, !Country %in% comparable_excl), "p_latest")

# ----------------------------------------------------------------------------
# 6. Joinpoint regression using the joinpointR package (v1.1.0), a wrapper
#    around segmented (Muggeo). Log-linear models: log(proportion) ~ year.
#    - Zero values cannot be log-transformed, so each series starts at the
#      first year with a non-zero proportion; series with < 4 non-zero points
#      are not modelled.
#    - Series with >= 7 observations: joinpointR::model_jp() with a maximum of
#      one joinpoint (k = 1), selected by the Bayesian Information Criterion
#      (segmented::selgmented, stepwise), minimum of 3 observations per segment
#      (package default, min_dist = 3), testing for a change in slope.
#    - Series with 4-6 observations: no joinpoint permitted (in line with NCI
#      guidance for short series); a single log-linear trend (lm) is fitted.
#    - APC per segment: joinpointR::get_apc() (segmented::slope, APC = TRUE).
#    - AAPC: segmented::aapc(exp.it = TRUE), so that AAPC is on the same
#      percentage scale as the APCs (joinpointR::get_aapc() reports the
#      un-exponentiated weighted slope for segmented models); for single-trend
#      models AAPC = APC = exp(slope) - 1 (as in joinpointR::get_aapc()).
# ----------------------------------------------------------------------------
library(joinpointR); library(segmented)
jpdat <- ts %>% filter(!is.na(prop)) %>% group_by(Country) %>% arrange(year, .by_group = TRUE) %>%
  filter(cumsum(prop > 0) > 0) %>%                        # start at first non-zero year
  filter(n() >= 4, all(prop > 0)) %>% mutate(n_obs = n()) %>% ungroup() %>%
  dplyr::select(Country, year, prop, n_obs) %>% as.data.frame()

long_series  <- filter(jpdat, n_obs >= 7)
short_series <- filter(jpdat, n_obs < 7)
mods_long  <- suppressMessages(model_jp(data = long_series, value = "prop", time = "year",
                                        group = "Country", k = 1))
mods_short <- lapply(split(short_series, short_series$Country),
                     function(s) lm(log(prop) ~ year, data = s))
jp_mods <- c(mods_long, mods_short)[sort(c(names(mods_long), names(mods_short)))]

jp_rows <- list(); jp_fits <- list()
for (cn in names(jp_mods)) {
  m <- jp_mods[[cn]]; s <- filter(jpdat, Country == cn)
  if (inherits(m, "segmented")) {
    psi  <- unname(m$psi[, "Est."])
    sl   <- slope(m, APC = TRUE)[[1]]                       # APC (%) and 95% CI per segment
    st   <- slope(m)[[1]]                                    # log-scale slope, SE, t
    seg_p <- 2 * pt(-abs(st[, "t value"]), df.residual(m))
    brk  <- c(min(s$year), psi, max(s$year))
    seg_txt <- paste0(fmt(brk[-length(brk)], 1), "-", fmt(brk[-1], 1), ": ",
                      fmt(sl[, 1], 1), " (", fmt(sl[, 2], 1), " to ", fmt(sl[, 3], 1), ")",
                      ifelse(seg_p < .05, "*", ""), collapse = "; ")
    a  <- aapc(m, exp.it = TRUE)                             # proportions, not %
    a_log <- aapc(m)                                         # log scale, for p-value
    aapc_est <- 100 * a[1]; aapc_l <- 100 * a[2]; aapc_u <- 100 * a[3]
    aapc_p <- 2 * pnorm(-abs(a_log[1] / a_log[2]))
    k <- length(psi); jp_year <- paste(fmt(psi, 1), collapse = "; ")
    fitted_vals <- exp(fitted(m))
  } else {
    b <- coef(summary(m))[2, ]; ci <- confint(m)[2, ]
    aapc_est <- 100 * (exp(b[1]) - 1); aapc_l <- 100 * (exp(ci[1]) - 1); aapc_u <- 100 * (exp(ci[2]) - 1)
    aapc_p <- b[4]; k <- 0; jp_year <- NA
    seg_txt <- paste0(min(s$year), "-", max(s$year), ": ", fmt(aapc_est, 1), " (", fmt(aapc_l, 1),
                      " to ", fmt(aapc_u, 1), ")", ifelse(aapc_p < .05, "*", ""))
    fitted_vals <- exp(fitted(m))
  }
  jp_fits[[cn]] <- data.frame(Country = cn, year = s$year, fit = fitted_vals)
  jp_rows[[cn]] <- data.frame(Country = cn, period = paste0(min(s$year), "-", max(s$year)),
                              n_obs = nrow(s), max_joinpoints = ifelse(nrow(s) >= 7, 1, 0), joinpoints = k,
                              joinpoint_year = jp_year, segments_APC = seg_txt,
                              AAPC = unname(aapc_est), AAPC_lcl = unname(aapc_l), AAPC_ucl = unname(aapc_u),
                              AAPC_p = unname(aapc_p),
                              AAPC_summary = paste0(fmt(aapc_est, 1), " (", fmt(aapc_l, 1), " to ", fmt(aapc_u, 1), ")"))
}
jp_tab <- bind_rows(jp_rows); rownames(jp_tab) <- NULL
write.csv(jp_tab, file.path(out, "SuppTable_joinpoint.csv"), row.names = FALSE)
# Package summary table for the long series (as produced by joinpointR)
write.csv(summary_jp(mods_long, digits = 1), file.path(out, "joinpointR_summary_long_series.csv"), row.names = FALSE)
View(jp_tab[, c("Country", "period", "joinpoints", "joinpoint_year", "segments_APC", "AAPC_summary", "AAPC_p")])


# --- R6.6: drop bootstrap CIs for epsilon-squared and Cramer's V -------------
# (percentile intervals for non-negative statistics have positive lower bounds
#  even under the null). Simplest fix: blank them before writing Tables 4 and 5.
tab4$lcl[tab4$test %in% c("Kruskal-Wallis", "Fisher exact")] <- NA
tab4$ucl[tab4$test %in% c("Kruskal-Wallis", "Fisher exact")] <- NA
tab4$summary <- sub(" \\(.*\\)$", "", tab4$summary)   # keep point estimate only

# --- R6.3: exclude all four countries reporting 0.0% in the latest year ------
zero_rep <- c("Chile", "Greece", "Korea", "Romania")
d_nz <- filter(d, !Country %in% zero_rep)
tab3_excl_zero <- assoc_table(d_nz, "p_latest", "continuous")
tab4_excl_zero <- assoc_table(d_nz, "uptake_cat", "categorical")

write.csv(tab3_excl_zero, file.path(out, "SupplTbl9_.csv"))

# --- R6.3 / R6.5: further policy sensitivity analyses -------------------------
d_ger <- d %>% mutate(dh_policy = ifelse(Country == "Germany", 1, dh_policy))
main_cuts <- c(5, 20.0000001)
sens_r2 <- bind_rows(
  policy_test(d_nz, main_cuts, "Excluding Chile, Greece, Korea and Romania"),
  policy_test(filter(d, !Country %in% c("Estonia", "Spain")), main_cuts,
              "Excluding Estonia and Spain (definition changes 2020/2021)"),
  policy_test(filter(d, !Country %in% c(comparable_excl, "Estonia", "Spain")), main_cuts,
              "Comparable and stable definitions"),
  policy_test(d_ger, main_cuts, "Germany recoded as having a strategy"),
  policy_test(filter(d_ger, !Country %in% c("Greece", "Romania")), main_cuts,
              "Germany recoded, excluding Greece and Romania"))
print(sens_r2)

# --- R6.2 / R6.4: joinpoint table checks --------------------------------------
# Residual degrees of freedom per model (report range in Methods):
jp_df <- sapply(jp_mods, df.residual); print(jp_df)
# Joinpoint locations on the continuous scale 
sapply(jp_mods, function(m) if (inherits(m, "segmented")) m$psi[, "Est."] else NA)

# ----------------------------------------------------------------------------
# 7. Longitudinal outcomes vs country characteristics
# ----------------------------------------------------------------------------
d <- d %>% left_join(jp_tab %>% dplyr::select(Country, AAPC), by = "Country")
long_change    <- assoc_table(d, "change_pp")
long_retention <- assoc_table(d, "retention")
long_aapc      <- assoc_table(d, "AAPC")
long_summary <- d %>% filter(!is.na(dh_policy)) %>% group_by(dh_policy) %>%
  summarise(n_change = sum(!is.na(change_pp)), median_change = median(change_pp, na.rm = TRUE),
            median_retention = median(retention, na.rm = TRUE),
            median_AAPC = median(AAPC, na.rm = TRUE), .groups = "drop")
print(long_summary)
print(rbind(change = long_change[long_change$variable == bin_vars["dh_policy"], ],
            retention = long_retention[long_retention$variable == bin_vars["dh_policy"], ],
            AAPC = long_aapc[long_aapc$variable == bin_vars["dh_policy"], ]))

# ----------------------------------------------------------------------------
# 8. Figures
# ----------------------------------------------------------------------------
# 8a. Figure 1: ranking ("bump") charts, 2015-2023, with darker background lines
bump <- function(metric, lab_fun, title, sub) {
  dd <- ts %>% filter(year <= 2023, !Country %in% c("Greece", "Romania", "Canada")) %>%
    mutate(val = .data[[metric]]) %>% filter(!is.na(val)) %>%
    group_by(year) %>% mutate(rank = rank(-val, ties.method = "first")) %>% ungroup()
  top <- dd %>% filter(year == 2023) %>% slice_min(rank, n = 6) %>% pull(Country)
  # smooth sigmoid between consecutive years
  sm <- dd %>% arrange(Country, year) %>% group_by(Country) %>%
    reframe({
      if (n() == 1) data.frame(x = year, y = rank) else {
        bind_rows(lapply(seq_len(n() - 1), function(i) {
          t <- seq(0, 1, length.out = 30); s <- 1 / (1 + exp(-12 * (t - .5)))
          s <- (s - min(s)) / (max(s) - min(s))
          data.frame(x = year[i] + t * (year[i + 1] - year[i]),
                     y = rank[i] + s * (rank[i + 1] - rank[i]))
        }))
      }
    })
  pal <- setNames(c("#1f77b4", "#ff7f0e", "#7f7f7f", "#d62728", "#9467bd", "#8c564b"), top)
  last <- dd %>% group_by(Country) %>% filter(year == max(year)) %>% ungroup()
  ggplot() +
    geom_line(data = filter(sm, !Country %in% top), aes(x, y, group = Country),
              colour = "grey45", linewidth = 0.6) +
    geom_point(data = filter(dd, !Country %in% top), aes(year, rank), colour = "grey45", size = 1.3) +
    geom_line(data = filter(sm, Country %in% top), aes(x, y, colour = Country), linewidth = 1.4) +
    geom_point(data = filter(dd, Country %in% top), aes(year, rank, colour = Country), size = 2.6) +
    geom_text(data = filter(dd, Country %in% top), aes(year, rank - 0.45, label = lab_fun(val),
                                                       colour = Country), size = 2.6, fontface = "bold") +
    geom_text(data = last, aes(max(dd$year) + 0.15, rank,
                               label = paste0(Country, " (", lab_fun(val), ")"),
                               colour = ifelse(Country %in% top, Country, NA)),
              hjust = 0, size = ifelse(last$Country %in% top, 3.4, 2.8),
              fontface = ifelse(last$Country %in% top, "bold", "plain"), na.rm = TRUE) +
    scale_colour_manual(values = pal, na.value = "grey30", guide = "none") +
    scale_y_reverse(breaks = NULL) + scale_x_continuous(breaks = 2015:2023, limits = c(2015, 2025.2)) +
    labs(title = title, subtitle = sub, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"),
          axis.text.x = element_text(face = "bold"))
}
pA <- bump("tc", function(v) fmt(v, 1), "(A) Teleconsultations per person per year",
           "Countries ranked each year; top 6 in 2023 highlighted")
pB <- bump("prop", function(v) paste0(fmt(v, 1), "%"),
           "(B) Teleconsultations as % of all physician consultations",
           "Countries ranked each year; top 6 in 2023 highlighted")
ggsave(file.path(out, "Figure1A.png"), pA, width = 8, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(out, "Figure1B.png"), pB, width = 8, height = 6.5, dpi = 300, bg = "white")

# Combined Figure 1 (A and B side by side) for the manuscript
jpeg(file.path(out, "Figure1_combined.jpeg"), width = 16, height = 7.4, units = "in", res = 300, quality = 95)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
print(pA, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(pB, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
invisible(dev.off())

# 8b. Supplementary Figure: observed proportions and fitted joinpoint models
obs  <- ts %>% filter(Country %in% names(jp_fits), !is.na(prop))
fits <- bind_rows(jp_fits)
pj <- ggplot() +
  geom_point(data = obs, aes(year, prop), size = 1.4, colour = 'black') +
  geom_line(data = fits, aes(year, fit), colour = "#c0392b", linewidth = 0.9) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = c(2015, 2019, 2023)) +
  labs(x = NULL, y = "Teleconsultations (% of physician consultations)",
       caption = "Points: observed. Red line: fitted log-linear joinpoint model (joinpointR), from first non-zero year.") +
  theme_bw(base_size = 9)

ggsave(file.path(out, "SuppFigure_joinpoint.png"), pj, width = 8, height = 9, dpi = 300, bg = "white")

# ----------------------------------------------------------------------------
# 9. Export all results to one workbook
# ----------------------------------------------------------------------------
for (nm in c("long_change", "long_retention", "long_aapc", "tab3_excl_GR_RO", "tab3_comparable"))
  write.csv(get(nm), file.path(out, paste0("SuppTable_", nm, ".csv")), row.names = FALSE)
wb <- createWorkbook()
for (nm in c("table1", "tab3", "tab4", "sens", "jp_tab", "long_change", "long_retention",
             "long_aapc", "tab3_excl_GR_RO", "tab3_comparable", "policy_lists")) {
  addWorksheet(wb, nm); writeData(wb, nm, get(nm))
}
saveWorkbook(wb, file.path(out, "TC_R1_results.xlsx"), overwrite = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out, "sessionInfo.txt"))

