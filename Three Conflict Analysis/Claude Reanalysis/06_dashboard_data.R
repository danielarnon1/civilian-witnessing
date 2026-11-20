# Builds the data for dashboard.html: descriptives, treatment effects (arm-by-arm and pooled by side,
# without and with controls), heterogeneity tests with BH correction, subgroup effects, and group means.
# Run from this folder after 00_prepare.R exists. Writes dashboard_data.json and injects it into dashboard.html
# (between the "// <<DATA" and "// DATA>>" lines).
#
# Heterogeneity (same method as the Genocide Perceptions analysis):
#   outcome ~ treatment * moderator, OLS with HC2 SEs; joint robust Wald test that all treatment x moderator
#   terms are zero; Benjamini-Hochberg q-values within survey, separately for each treatment family
#   (6 arms; pooled sides). Flagged as significant when q < .10.

source("00_prepare.R")
suppressMessages({ library(estimatr); library(jsonlite) })

Q_CUT <- 0.10
MIN_CELL <- 20   # a moderator level enters a heterogeneity test only if every treatment group has >= 20 respondents in it

# keep only moderator levels with >= MIN_CELL respondents in every treatment group
keep_levels <- function(x, treat, mod) {
  tb <- table(x[[mod]], x[[treat]])
  rownames(tb)[apply(tb, 1, min) >= MIN_CELL]
}

OUTCOMES <- tribble(
  ~key,           ~label,                                   ~short,                ~scale,      ~dir,
  "symp_A",       "Q5 Sympathies",                          "Sympathy (1-5)",      "1-5 scale", "Higher = more sympathy toward A",
  "justified",    "Q6 A's military actions are justified",  "Actions justified",   "share",     "Share saying A's actions are justified (don't know excluded)",
  "dk_justified", "Q6 Don't know enough",                   "Q6 don't know",       "share",     "Share answering 'don't know enough' on Q6",
  "us_lenient",   "Q7 U.S. approach toward A",              "U.S. approach (1-3)", "1-3 scale", "1 = reduce aid, 2 = diplomatic pressure, 3 = no pressure (don't know excluded)",
  "dk_us",        "Q7 Don't know enough",                   "Q7 don't know",       "share",     "Share answering 'don't know enough' on Q7",
  "attn_little",  "Q8 Leaders give this conflict too little attention", "Too little attention", "share", "Share saying U.S. leaders give this conflict too little attention",
  "ceasefire",    "Q10 Importance of a ceasefire / peace",  "Ceasefire (1-5)",     "1-5 scale", "1 = not at all important, 5 = extremely important",
  "humanit",      "Q11 Importance of humanitarian relief to B", "Humanitarian (1-5)", "1-5 scale", "1 = not at all important, 5 = extremely important",
  "milaid_A",     "Q12 Importance of military aid to A",    "Military aid to A (1-5)", "1-5 scale", "1 = not at all important, 5 = extremely important"
)

DISTS <- list(
  q1 = list(label = "Q1 How often do you see posts like this one?", control = FALSE,
            levels = c("Never seen", "Rarely, if ever", "A few times/week", "At least once/day", "A few times/day"), pal = "seq"),
  q2 = list(label = "Q2 How likely would you be to share this post?", control = FALSE,
            levels = c("Not likely at all", "Not very likely", "Somewhat likely", "Very likely"), pal = "seq"),
  q5 = list(label = "Q5 Sympathies lie...", control = TRUE,
            levels = c("Entirely A", "Mostly A", "Equally with both", "Mostly B", "Entirely B", "With neither", "Not sure"), pal = "div"),
  q6 = list(label = "Q6 A's military action...", control = TRUE,
            levels = c("Defending its interests; justified", "Has gone too far; not justified", "Don't know enough"), pal = "ab"),
  q7 = list(label = "Q7 The U.S. should...", control = TRUE,
            levels = c("Let A pursue its policy, no pressure", "Diplomatic pressure, keep aid", "Reduce military aid", "Don't know enough"), pal = "abm"),
  q8 = list(label = "Q8 U.S. leaders' attention to this conflict", control = TRUE,
            levels = c("Too little", "Right amount", "Too much"), pal = "att"),
  ceasefire = list(label = "Q10 Importance of a ceasefire / peace", control = TRUE,
                   levels = c("Not at all", "Slightly", "Moderately", "Very", "Extremely"), pal = "seq"),
  humanit   = list(label = "Q11 Importance of humanitarian relief to B", control = TRUE,
                   levels = c("Not at all", "Slightly", "Moderately", "Very", "Extremely"), pal = "seq"),
  milaid_A  = list(label = "Q12 Importance of military aid to A", control = TRUE,
                   levels = c("Not at all", "Slightly", "Moderately", "Very", "Extremely"), pal = "seq")
)

arm_short <- function(x) sub("\n", " ", x)
pct <- function(x) round(100 * x, 1)

# ------------------------------------------------------------------ helpers
fit_effects <- function(d, v, treat, controls) {
  rhs <- c(treat, if (controls) c(CONTROLS, "m_prior_f"))
  dd <- d %>% filter(!is.na(.data[[v]]))
  if (controls) dd <- dd %>% filter(if_all(all_of(CONTROLS), ~ !is.na(.x)))
  m <- lm_robust(reformulate(rhs, v), data = dd, se_type = "HC2")
  ci <- confint(m)
  k <- grep(paste0("^", treat), names(coef(m)))
  tibble(g = sub(paste0("^", treat), "", names(coef(m))[k]), e = coef(m)[k], lo = ci[k, 1], hi = ci[k, 2],
         p = m$p.value[k]) %>% mutate(n = nobs(m))
}

wald_inter <- function(m, treat, mod) {
  # interaction terms only: "<treat><level>:<mod><level>" (level labels may themselves contain ":")
  k <- grep(paste0("^", treat, ".+:", mod), names(coef(m)))
  k <- k[!is.na(coef(m)[k])]
  b <- coef(m)[k]; V <- vcov(m)[k, k, drop = FALSE]
  ok <- is.finite(diag(V)) & diag(V) > 0          # drop terms with degenerate SEs (constant outcome in a tiny cell)
  b <- b[ok]; V <- V[ok, ok, drop = FALSE]
  if (length(b) == 0) return(c(stat = NA, df = 0, p = NA))   # no estimable interaction terms
  # generalized inverse handles near-singular V; df = rank of V
  stat <- as.numeric(t(b) %*% MASS::ginv(V) %*% b)
  r <- qr(V)$rank
  c(stat = stat, df = r, p = pchisq(stat, r, lower.tail = FALSE))
}

mean_ci <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 2) return(c(m = NA, lo = NA, hi = NA, n = n))
  se <- sd(x) / sqrt(n); c(m = mean(x), lo = mean(x) - 1.96 * se, hi = mean(x) + 1.96 * se, n = n)
}

# ------------------------------------------------------------------ per survey
build_survey <- function(ttl) {
  s  <- surveys[surveys$title == ttl, ]
  d  <- dd_all %>% filter(title == ttl) %>%
    mutate(arm_s = arm_short(arm),
           arm_f = relevel(factor(arm_s), ref = "Control"),
           side_f = relevel(factor(side), ref = "Control"),
           m_prior_f = factor(ifelse(is.na(m_prior), "missing", m_prior)))
  arm_order <- c("Control", arm_short(arm_levels[grepl(paste0("^(", s$A_lab, "|", s$B_lab, ")\n"), arm_levels)]))
  treat_arms <- arm_order[-1]
  sideAB <- c(A = s$A_lab, B = s$B_lab)
  cat("==", ttl, "\n")

  # ---- descriptives: response distributions
  dist <- imap(DISTS, function(cfg, v) {
    x <- d %>% filter(!is.na(.data[[v]]))
    rows <- list(list(g = "All treated", n = sum(x$side != "Control")))
    groups <- c("All treated", if (cfg$control) "Control", treat_arms)
    map(groups, function(g) {
      y <- if (g == "All treated") x %>% filter(side != "Control") else x %>% filter(arm_s == g)
      tb <- tabulate(y[[v]], nbins = length(cfg$levels))
      list(g = g, n = nrow(y), pct = pct(tb / max(1, nrow(y))))
    }) %>% list(label = cfg$label, levels = cfg$levels, pal = cfg$pal, rows = .)
  })

  # ---- sample composition + balance across the 7 arms
  sample <- map(MODERATORS$mod, function(m) {
    lv <- MODERATORS$levels[MODERATORS$mod == m][[1]]
    x <- d %>% filter(!is.na(.data[[m]]))
    lv <- lv[lv %in% x[[m]]]
    if (length(lv) == 0)   # question not asked in this survey
      return(list(mod = m, levels = list(), n = list(), pct = list(), by_arm = list(), balance_p = NA, missing = nrow(d)))
    tb <- table(factor(x[[m]], lv), x$arm_f)
    bal <- suppressWarnings(chisq.test(tb)$p.value)
    list(mod = m, levels = lv, n = as.integer(rowSums(tb)), pct = pct(rowSums(tb) / sum(tb)),
         by_arm = map(arm_order, function(a) pct(tb[, a] / sum(tb[, a]))), balance_p = bal,
         missing = sum(is.na(d[[m]])))
  })

  # ---- treatment effects: arm-by-arm and pooled, no controls / controls
  te <- map(OUTCOMES$key, function(v) {
    ctrl_mean <- mean(d[[v]][d$side == "Control"], na.rm = TRUE)
    one <- function(treat, ctrl) {
      r <- fit_effects(d, v, treat, ctrl)
      list(n = r$n[1], g = r$g, e = r$e, lo = r$lo, hi = r$hi, p = r$p)
    }
    list(ctrl_mean = ctrl_mean,
         arm  = list(no = one("arm_f", FALSE),  ctrl = one("arm_f", TRUE)),
         side = list(no = one("side_f", FALSE), ctrl = one("side_f", TRUE)))
  }) %>% set_names(OUTCOMES$key)
  # reorder arm results to arm_order
  te <- map(te, function(o) {
    o$arm <- map(o$arm, function(r) { i <- match(treat_arms, r$g); r$g <- r$g[i]; r$e <- r$e[i]; r$lo <- r$lo[i]; r$hi <- r$hi[i]; r$p <- r$p[i]; r })
    o$side <- map(o$side, function(r) { i <- match(c("A", "B"), r$g); r$g <- unname(sideAB[r$g[i]]); r$e <- r$e[i]; r$lo <- r$lo[i]; r$hi <- r$hi[i]; r$p <- r$p[i]; r })
    o
  })

  # ---- heterogeneity tests
  het <- map_dfr(c(arm = "arm_f", side = "side_f"), function(treat) {
    expand_grid(dv = OUTCOMES$key, mod = MODERATORS$mod) %>% pmap_dfr(function(dv, mod) {
      x <- d %>% filter(!is.na(.data[[dv]]), !is.na(.data[[mod]]))
      lv <- keep_levels(x, treat, mod)
      if (length(lv) < 2) return(tibble(dv = dv, mod = mod, stat = NA, df = 0, p = NA, n = 0, levels = ""))
      x <- x %>% filter(.data[[mod]] %in% lv)
      m <- lm_robust(reformulate(paste(treat, "*", mod), dv), data = x, se_type = "HC2")
      w <- wald_inter(m, treat, mod)
      tibble(dv = dv, mod = mod, stat = w[["stat"]], df = w[["df"]], p = w[["p"]], n = nobs(m),
             levels = paste(lv, collapse = " | "))
    })
  }, .id = "family") %>%
    group_by(family) %>% mutate(q = p.adjust(p, "BH")) %>% ungroup()

  # ---- subgroup effects + group means for significant pairs (either family)
  sig <- het %>% filter(q < Q_CUT)
  sub <- pmap(sig, function(family, dv, mod, levels, ...) {
    lv <- MODERATORS$levels[MODERATORS$mod == mod][[1]]
    lv <- lv[lv %in% strsplit(levels, " | ", fixed = TRUE)[[1]]]    # only the levels that entered the test
    treat <- if (family == "arm") "arm_f" else "side_f"
    groups <- if (family == "arm") treat_arms else unname(sideAB)
    eff <- map(lv, function(l) {
      x <- d %>% filter(.data[[mod]] == l)
      r <- fit_effects(x, dv, treat, FALSE)
      if (family == "side") r$g <- unname(sideAB[r$g])
      i <- match(groups, r$g)
      list(level = l, n = r$n[1], e = r$e[i], lo = r$lo[i], hi = r$hi[i], p = r$p[i])
    })
    gcol <- if (family == "arm") "arm_s" else "side"
    gmap <- if (family == "arm") setNames(c("Control", treat_arms), c("Control", treat_arms)) else c(Control = "Control", A = s$A_lab, B = s$B_lab)
    means <- map(lv, function(l) {
      x <- d %>% filter(.data[[mod]] == l)
      mc <- map(names(gmap), function(gk) mean_ci(x[[dv]][x[[gcol]] == gk]))
      list(level = l, m = map_dbl(mc, "m"), lo = map_dbl(mc, "lo"), hi = map_dbl(mc, "hi"), n = map_dbl(mc, "n"))
    })
    list(family = family, dv = dv, mod = mod, groups = groups, mean_groups = unname(gmap), eff = eff, means = means)
  })

  # ---- descriptives by demographic group (all outcomes x moderators), Control / A posts / B posts
  bygroup <- map(OUTCOMES$key, function(v) {
    map(MODERATORS$mod, function(mod) {
      lv <- MODERATORS$levels[MODERATORS$mod == mod][[1]]
      lv <- lv[lv %in% d[[mod]]]
      x <- d %>% filter(!is.na(.data[[v]]), !is.na(.data[[mod]]))
      rows <- map(lv, function(l) {
        y <- x %>% filter(.data[[mod]] == l)
        mc <- map(c("Control", "A", "B"), function(sd_) mean_ci(y[[v]][y$side == sd_]))
        all <- mean_ci(y[[v]])
        list(level = l, n = nrow(y), all = unname(all[1:3]),
             m = map_dbl(mc, "m"), lo = map_dbl(mc, "lo"), hi = map_dbl(mc, "hi"), ng = map_dbl(mc, "n"))
      })
      # descriptive test: does the outcome differ across levels (all respondents)? one-way ANOVA
      pa <- tryCatch(anova(lm(reformulate(mod, v), data = x))[["Pr(>F)"]][1], error = function(e) NA)
      list(mod = mod, overall = mean(x[[v]]), p = pa, rows = rows)
    }) %>% set_names(MODERATORS$mod)
  }) %>% set_names(OUTCOMES$key)

  # ---- open-ended Q3 word counts (from 09_open_text.R)
  words <- NULL
  if (file.exists("open_text_words.csv")) {
    w <- read.csv("open_text_words.csv") %>% filter(title == ttl)
    words <- map(c(all = "All posts", A = "A", B = "B"), function(g) {
      x <- w %>% filter(group == g) %>% arrange(desc(n))
      list(N = x$N[1], word = x$word, n = x$n, share = round(x$share, 4))
    })
  }

  list(key = s$survey, title = ttl, A = s$A_lab, B = s$B_lab, n = nrow(d),
       n_arm = as.integer(table(factor(d$arm_s, arm_order))), arms = arm_order,
       fielded = paste(range(d$year_resp), collapse = "-"),
       dist = dist, sample = sample, te = te,
       het = het %>% mutate(across(c(stat, p, q), ~ signif(.x, 4))),
       sub = sub, bygroup = bygroup, words = words)
}

out <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  q_cut = Q_CUT, min_level_n = MIN_LEVEL_N,
  outcomes = OUTCOMES,
  moderators = MODERATORS %>% select(mod, label, group, item),
  controls = c("Age", "Female", "White", "4-year degree", "Income (8 brackets)", "Party ID (7-pt)", "Ideology (7-pt)",
               "Religious attendance", "Social media use", "Followed this conflict", "Prior sympathy in this conflict"),
  surveys = map(surveys$title, build_survey) %>% set_names(surveys$survey)
)

json <- toJSON(out, auto_unbox = TRUE, digits = 4, na = "null", null = "null")
writeLines(json, "dashboard_data.json")
cat("saved dashboard_data.json (", round(nchar(json) / 1024), " KB)\n", sep = "")

# ---- inject into dashboard.html
if (file.exists("dashboard.html")) {
  h <- readLines("dashboard.html", warn = FALSE, encoding = "UTF-8")
  a <- grep("^// <<DATA", h); b <- grep("^// DATA>>", h)
  stopifnot(length(a) == 1, length(b) == 1, b > a)
  h <- c(h[1:a], paste0("const DATA = ", json, ";"), h[b:length(h)])
  writeLines(h, "dashboard.html", useBytes = TRUE)
  cat("injected data into dashboard.html\n")
}

# ---- console summary of significant heterogeneity
het_all <- map_dfr(out$surveys, ~ .x$het %>% mutate(survey = .x$title))
write.csv(het_all, "heterogeneity_tests.csv", row.names = FALSE)
cat("\nTests with BH q <", Q_CUT, "\n")
het_all %>% filter(q < Q_CUT) %>% arrange(survey, family, q) %>%
  select(survey, family, dv, mod, df, p, q) %>% as.data.frame() %>% print(row.names = FALSE)
cat("\nCount of tests by survey and family:\n")
het_all %>% count(survey, family, sig = q < Q_CUT) %>% as.data.frame() %>% print(row.names = FALSE)
