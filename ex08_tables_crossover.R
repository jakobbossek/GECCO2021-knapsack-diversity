library(tidyverse)
library(kableExtra)
devtools::load_all("/Users/bossek/repos/software/r/tblutils")

source("study1/defs.R")

table_dir = "study1/tables"
if (!dir.exists(table_dir))
  dir.create(table_dir)

tbl = readr::read_delim("study1/data/final.csv", delim = " ")

# with reduced budget
# tbl = readr::read_delim(file.path(dir_data, "evals.csv"), delim = " ")
# tbl$entropy = tbl$entropy_mu
# tbl$entropy_mu = NULL

print(tbl)

# global variables
alpha = 0.05

# filter and prepare data
tbltidy = tbl %>%
  filter(mutator %in% c("abf", "bf", "pbf")) %>%
  filter(mu %in% c(25, 100)) %>%
  filter(eps %in% c(0.1, 0.5, 0.9)) %>%
  filter(generator != "invscorr") %>%
  filter(repair == "deterministic") %>%
  select(generator, D, mu, eps, mutator, crossover, entropy)

rename = c("abf" = "EDO-BBF1", "sbf" = "EDO-BBF2", "bf" = "BF", "pbf" = "PBF", "htbf" = "HTBF")

# make algorithms factors
tbltidy$mutator = sapply(tbltidy$mutator, function(x) rename[x])
tbltidy$mutator = factor(tbltidy$mutator, levels = c("BF", "PBF", "EDO-BBF1"), ordered = TRUE)
tbltidy$crossover = factor(tbltidy$crossover, levels = c("off", "on"), ordered = TRUE)

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

mutators = unique(tbltidy$mutator)

tbls = lapply(mutators, function(MUT) {
  tmp = filter(tbltidy, mutator == MUT)
  tmp$crossover2 = as.integer(tmp$crossover)
  tmp = tblutils::test.pairwise(tmp, by = c("generator", "D", "mu", "eps"),
    split.col = "crossover2", value.col = "entropy", testresult.col = "stat",
    alternative = "greater", alpha = alpha, show.positive.only = FALSE, show.numbers.only = FALSE, show.intervals = FALSE)
  tmp$crossover2 = NULL
  tmp = tmp %>%
    group_by(generator, D, mu, eps, crossover) %>%
    dplyr::summarize(mean = mean(entropy), sd = sd(entropy), stat = stat[1L]) %>%
    ungroup() %>%
    arrange(generator, D, mu, eps, crossover)

  tmp$mean = round(tmp$mean, 2L)
  tmp$sd = round(tmp$sd, 2L)
  tmp = tblutils::highlight(tmp, by = c("generator", "D", "mu", "eps"), which = "mean",
    highlight.fun = baseLaTeXHighlighter, bg.color = "gray", bg.saturation.max = 20, order.fun = "max")
  tmp = tblutils::widen(tmp, split.col = "crossover", widen.cols = c("mean", "sd", "stat"))
  tmp$crossover = NULL
  arrange(tmp, generator, D, mu, eps)
})

# get BF, then PBF then EDO-BBF1
tbl_bf = tbls[[1]]
tbl_pbf = tbls[[3]]
tbl_edobbf1 = tbls[[2]]

tblfinal = cbind(tbl_bf, tbl_pbf[, 5:10], tbl_edobbf1[, 5:10])

toTableStar = function(x, pos = "htbp") {
  x = gsub("begin{table}", sprintf("begin{table*}[%s]", pos), x, fixed = TRUE)
  x = gsub("end{table}", "end{table*}", x, fixed = TRUE)
  return(x)
}

create_table = function(tbl, alpha = 0.05) {
  cns = c("", "$D$", "$\\mu$", "$\\varepsilon$", rep(c("\\textbf{mean}", "\\textbf{std}", "\\textbf{stat}"), 6L))
  align = c("c", rep("r", ncol(tbl)))
  ktbl = kableExtra::kable(tbl, "latex", col.names = cns, align = align, longtable = FALSE, escape = FALSE, booktabs = TRUE,
    caption = sprintf("Mean (\\text{mean}), standard deviation (\\text{std}) and results of Wilcoxon-Mann-Whitney tests at a signficance level of $\\alpha = %.2f$ (\\textbf{stat}) in terms of mean entropy. Best, i.e. lowest, mean values are highlighted in \\colorbox{gray!20}{\\textbf{bold face}}.", alpha)) %>%
    kable_styling() %>%
    row_spec(row = c(6, 12, 24, 30, 42, 48), extra_latex_after = "\\cmidrule{2-22}") %>%
    row_spec(row = setdiff(seq(3, nrow(tbl) - 1, 3), c(6, 12, 24, 30, 42, 48)), extra_latex_after = "\\cmidrule{3-22}") %>%
    collapse_rows(columns = 1:3, latex_hline = "major", valign = "middle") %>%
    add_header_above(c(" ", " ", " ", " ", "no-CO (1)" = 3, "CO (2)" = 3, "no-CO (1)" = 3, "CO (2)" = 3, "no-CO (1)" = 3, "CO (2)" = 3), bold = TRUE) %>%
    add_header_above(c(" ", " ", " ", " ", "BF" = 6, "PBF" = 6, "EDO-BBF1" = 6), bold = TRUE) %>%
    kable_styling(latex_options = c("repeat_header"))
  #print(dim(tbl))
  ktbl = as.character(ktbl)
  toTableStar(ktbl)
}

polish = function(x, to_rotate = NULL) {
  x = gsub("\\begin{tabular", "\\begin{scriptsize}\\begin{tabular", x, fixed = TRUE)
  x = gsub("end{tabular}", "end{tabular}\\end{scriptsize}", x, fixed = TRUE)
  return(x)
}

kabled = polish(create_table(tblfinal))
cat(kabled, file = file.path(table_dir, "crossover.tex"))
#cat(kabled, file = file.path(table_dir, "entropy_uncorr_usw.tex"))
