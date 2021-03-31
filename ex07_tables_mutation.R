library(tidyverse)
library(kableExtra)
devtools::load_all("/Users/bossek/repos/software/r/tblutils")

source("study1/defs.R")

table_dir = "study1/tables"
if (!dir.exists(table_dir))
  dir.create(table_dir)

tbl = readr::read_delim("study1/data/final.csv", delim = " ")

# with reduced budget
# tbl = readr::read_delim("study1/data/evals.csv", delim = " ")
# tbl$entropy = tbl$entropy_mu
# tbl$entropy_mu = NULL

print(tbl)

# global variables
alpha = 0.05

# filter and prepare data
tbltidy = tbl %>%
  filter(crossover == "off") %>%
  filter(mu %in% c(25, 100)) %>%
  filter(eps %in% c(0.1, 0.5, 0.9)) %>%
  filter(mutator != "bbf") %>%
  filter(generator != "invscorr") %>%
  filter(repair == "deterministic") %>%
  select(generator, D, mu, eps, mutator, entropy)

rename = c("abf" = "EDO-BBF1", "sbf" = "EDO-BBF2", "bf" = "BF", "pbf" = "PBF", "htbf" = "HTBF")

# make algorithms factors
tbltidy$mutator = sapply(tbltidy$mutator, function(x) rename[x])
tbltidy$mutator = factor(tbltidy$mutator, levels = c("BF", "PBF", "HTBF", "EDO-BBF1", "EDO-BBF2"), ordered = TRUE)

tbltidy$mutator2 = as.integer(tbltidy$mutator)
tbltidy = tblutils::test.pairwise(tbltidy, by = c("generator", "D", "mu", "eps"),
  split.col = "mutator2", value.col = "entropy", testresult.col = "stat",
  alternative = "greater", alpha = alpha, show.positive.only = TRUE, show.numbers.only = TRUE, show.intervals = TRUE)
tbltidy$mutator2 = NULL

tbltidyaggr = tbltidy %>%
  group_by(generator, D, mu, eps, mutator) %>%
  dplyr::summarize(mean = mean(entropy), sd = sd(entropy), stat = stat[1L]) %>%
  ungroup() %>%
  arrange(generator, D, mu, eps, mutator)

# Statistics on standard deviation
tbltidyaggr %>%
  group_by(mutator) %>%
  dplyr::summarize(maxsd = max(sd), q09sd = quantile(sd, probs = 0.9))

tt = tbltidyaggr %>%
  group_by(generator, D, mu, eps) %>%
  dplyr::filter(mutator != "EDO-BBF2", mean != 0) %>%
  dplyr::mutate(meanmax = max(mean)) %>%
  dplyr::mutate(meanscaled = round(100 * (meanmax - mean) / meanmax, 2)) %>%
  dplyr::summarize(meandiff = max(meanscaled)) %>%
  filter(meandiff < 100 && !is.nan(meandiff)) %>%
  select(meandiff)
as.data.frame(tt)


toTableStar = function(x, pos = "htbp") {
  x = gsub("begin{table}", sprintf("begin{table*}[%s]", pos), x, fixed = TRUE)
  x = gsub("end{table}", "end{table*}", x, fixed = TRUE)
  return(x)
}

baseLaTeXHighlighter = function(values, ordering, n.highlight = 1,
  bg.color = "gray", bg.saturation.max = 80, bg.saturation.min = 20) {
  n.values = length(values)
  # transform to nicely formated characters
  values = sprintf("%.2f", values)

  # highlight best value in bold
  max.val.idx = which(ordering == 1L)
  values[max.val.idx] = sprintf("\\textbf{%s}", values[max.val.idx])

  # Now for the cellcolors
  col.values = rep(bg.saturation.max, n.values)

  # ordering - 1 achieves no desaturation for best value
  desaturation.factor = ceiling((bg.saturation.max - bg.saturation.min) / n.highlight)
  # FIXME: magic number 20
  col.values = col.values - (ordering - 1L) * 20
  if (n.highlight > n.values)
    n.highlight = n.values

  # which elements shall be highlighted
  idx = which(ordering <= n.highlight)

  col.values = sprintf("%s!%i", bg.color, col.values)
  values[idx] = sprintf("\\cellcolor{%s}{%s}", col.values[idx], values[idx])
  return(values)
}


create_table = function(tbl, alpha = 0.05) {
  n_algos = length(unique(tbltidyaggr$mutator))
  tbl$mean = round(tbl$mean, 2L)
  tbl$sd = round(tbl$sd, 2L)
  tbl = tblutils::highlight(tbl, by = c("generator", "D", "mu", "eps"), which = "mean",
    highlight.fun = baseLaTeXHighlighter, bg.color = "gray", bg.saturation.max = 20, order.fun = "max")
  tbl = tblutils::widen(tbl, split.col = "mutator", widen.cols = c("mean", "sd", "stat"))
  tbl$mutator = NULL
  tbl = arrange(tbl, generator, D, mu, eps)

  cns = c("", "$D$", "$\\mu$", "$\\varepsilon$", rep(c("\\textbf{mean}", "\\textbf{std}", "\\textbf{stat}"), n_algos))
  align = c("c", rep("r", ncol(tbl)))
  ktbl = kableExtra::kable(tbl, "latex", col.names = cns, align = align, longtable = FALSE, escape = FALSE, booktabs = TRUE,
    caption = sprintf("Mean (\\text{mean}), standard deviation (\\text{std}) and results of Wilcoxon-Mann-Whitney tests at a signficance level of $\\alpha = %.2f$ (\\textbf{stat}) in terms of mean entropy. Best, i.e. lowest, mean values are highlighted in \\colorbox{gray!20}{\\textbf{bold face}}.", alpha)) %>%
    kable_styling() %>%
    row_spec(row = c(6, 12, 24, 30, 42, 48), extra_latex_after = "\\cmidrule{2-19}") %>%
    row_spec(row = setdiff(seq(3, nrow(tbl) - 1, 3), c(6, 12, 24, 30, 42, 48)), extra_latex_after = "\\cmidrule{3-19}") %>%
    collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle") %>%
    add_header_above(c(" ", " ", " ", " ", "BF (1)" = 3, "PBF (2)" = 3, "HTBF (3)" = 3, "EDO-BBF1 (4)" = 3, "EDO-BBF2 (5)" = 3), bold = TRUE) %>%
    add_header_above(c(" ", " ", " ", " ", "Standard" = 9, "Biased" = 6), bold = TRUE) %>%
    kable_styling(latex_options = c("repeat_header"))# %>%
    #footnote(general = "The \\textbf{stat} columns contain results of Wilcoxon-Mann-Whitney tests a a significance level of $\\alpha = 5%$. A value of e.g. 3 means that the zero-hypothesis of greater median was rejected for algorithm 3. Analogously, interval notation 3-5 means that the zeero hypothesis was rejected for algorithms 3, 4 and 5.", threeparttable = TRUE, escape = FALSE)

  #print(dim(tbl))
  ktbl = as.character(ktbl)
  toTableStar(ktbl)
}

polish = function(x, to_rotate = NULL) {
  x = gsub("\\begin{tabular", "\\begin{scriptsize}\\begin{tabular", x, fixed = TRUE)
  x = gsub("end{tabular}", "end{tabular}\\end{scriptsize}", x, fixed = TRUE)

  # for (pat in to_rotate) {
  #   x = gsub(pat, sprintf("\\rotate[origin=c]{-90}{%s}", pat), x, fixed = TRUE)
  # }
  return(x)
}

kabled = polish(create_table(tbltidyaggr))
cat(kabled, file = file.path(table_dir, "entropy_mutation_only_with_repair.tex"))
#cat(kabled, file = file.path(table_dir, "entropy_uncorr_usw.tex"))
