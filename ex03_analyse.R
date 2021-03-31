library(tidyverse)
library(ggplot2)

source("study1/defs.R")

image_dir = "study1/images"
if (!dir.exists(image_dir))
  dir.create(image_dir)

tbl = readr::read_delim("study1/data/final.csv", delim = " ")
print(tbl)

tbl = tbl %>%
  arrange(generator, D, eps, mu) %>%
  filter(mutator != "bbf")

rename = c("abf" = "EDO-BBF1", "sbf" = "EDO-BBF2", "bf" = "BF", "pbf" = "PBF", "htbf" = "HTBF")

# make algorithms factors
tbl$mutator = sapply(tbl$mutator, function(x) rename[x])
tbl$mutator = factor(tbl$mutator, levels = c("BF", "PBF", "HTBF", "EDO-BBF1", "EDO-BBF2"), ordered = TRUE)

tbl$facet_var = sprintf("(%s, %i, %i, %.2f)", tbl$generator, tbl$D, tbl$mu, tbl$eps)
tbl$repair = recode_factor(tbl$repair, `deterministic` = "on" , `off` = "off")

tbl1 = filter(tbl, generator == "invscorr", D == 10, mu == 25, eps == 0.5)
tbl2 = filter(tbl, generator == "invscorr", D == 2, mu == 25, eps == 0.1)
tbl3 = filter(tbl, generator == "scorr", D == 2, mu == 100, eps == 0.5)
tbl4 = filter(tbl, generator == "scorr", D == 2, mu == 25, eps == 0.9)
tbl5 = filter(tbl, generator == "uncorr", D == 5, mu == 25, eps == 0.1)
tbl6 = filter(tbl, generator == "usw", D == 5, mu == 25, eps == 0.1)

tblpaper = rbind(tbl1, tbl2, tbl3, tbl4, tbl5, tbl6) %>%
  filter(crossover == "off")

g = ggplot(tblpaper, aes(x = mutator, y = entropy))
g = g + geom_boxplot(aes(color = repair), alpha = 0.5)
g = g + theme_bw()
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 25), legend.margin = margin(0,0,0,0), legend.box.margin = margin(-8,-8,-8,-8))
g = g + guides(colour = guide_legend(nrow = 1))
g = g + labs(
  #title = "Distribution of population diversity for different mutators",
  #subtitle = sprintf("Repair mechanism: %s", REPAIR),
  y = "Entropy",
  x = "Repair",
  color = "Repair")
#g = g + facet_grid(generator + D ~ eps + mu, scales = "free_y")
g = g + facet_wrap(. ~ facet_var, scales = "free_y", ncol = 3L)
fn = file.path(image_dir, "boxplots_repair_on-off.pdf")
ggsave(fn, width = 8, height = 4, limitsize = FALSE)


tbl2 = tbl %>% group_by(repair, mutator) %>%
  dplyr::summarise(
    no_improve = round(mean(iters_without_improvement / iters) * 100, digits = 2),
    did_repair = round(mean(repair_operations / (iters)) * 100, digits = 2)) %>%
  arrange(no_improve) %>%
  ungroup() %>%
  reshape2::melt(id.vars = c("repair", "mutator"), variable.name = "split", value.name = "value")

g = ggplot(tbl2, aes(x = mutator, y = value, fill = split))
g = g + geom_bar(stat = "identity", position = "dodge", alpha = 0.5)
g = g + theme_bw()
g = g + scale_fill_brewer(palette = "Dark2")
#g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
g = g + facet_grid(. ~ repair)


stop()

# THE BIG PICTURE
# ===
for (REPAIR in unique(tbl$repair)) {
  g = ggplot(filter(tbl, repair == REPAIR), aes(x = interaction(crossover, mutator, sep = " / "), y = entropy))
  g = g + geom_boxplot(aes(color = mutator), alpha = 0.5)
  g = g + theme_bw()
  g = g + scale_color_brewer(palette = "Dark2")
  g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
  g = g + guides(colour = guide_legend(nrow = 1))
  g = g + labs(
    #title = "Distribution of population diversity for different mutators",
    #subtitle = sprintf("Repair mechanism: %s", REPAIR),
    ylab = "Entropy",
    xlab = "Mutator",
    color = "Mutator")
  #g = g + facet_grid(generator + D ~ eps + mu, scales = "free_y")
  g = g + facet_wrap(. ~ facet_var, scales = "free_y", ncol = 6L)
  fn = file.path(image_dir, sprintf("boxplots_mutator_crossover_repair_%s.pdf", REPAIR))
  ggsave(fn, width = 14, height = 80, limitsize = FALSE)
}

# OK, crossover sucks. So we go for biased mutation operators only.
tblmut = filter(tbl, crossover == "off")
g = ggplot(tblmut, aes(x = interaction(repair, mutator, sep = " / "), y = entropy))
g = g + geom_boxplot(aes(color = mutator), alpha = 0.5)
g = g + theme_bw()
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
g = g + guides(colour = guide_legend(nrow = 1))
g = g + labs(
  #title = "Distribution of population diversity for different mutators",
  #subtitle = sprintf("Repair mechanism: %s", REPAIR),
  ylab = "Entropy",
  xlab = "Mutator",
  color = "Mutator")
#g = g + facet_grid(generator + D ~ eps + mu, scales = "free_y")
g = g + facet_wrap(. ~ facet_var, scales = "free_y", ncol = 6L)
fn = file.path(image_dir, "boxplots_mutator_repair.pdf")
ggsave(fn, width = 14, height = 80, limitsize = FALSE)

stop("DONE")

tbl2 = tbl
#tbl2$entropy = log(tbl2$entropy)
g = ggplot(tbl2, aes(x = interaction(repair, mutator, sep = " / "), y = entropy))
g = g + geom_boxplot(aes(color = mutator), alpha = 0.5)
#g = g + scale_y_log10()
g = g + ylim(c(25, 38))
g = g + theme_bw()
g = g + scale_color_brewer(palette = "Dark2")
g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
g = g + guides(colour = guide_legend(nrow = 1))
g = g + labs(
  #title = "Distribution of population diversity for different mutators",
  #subtitle = sprintf("Repair mechanism: %s", REPAIR),
  ylab = "Entropy",
  xlab = "Mutator / Repair",
  color = "Mutator")
#g = g + facet_grid(generator + D ~ eps + mu, scales = "free_y")
g = g + facet_wrap(. ~ facet_var, scales = "free_y", ncol = 3L)
fn = file.path(image_dir, "boxplots_mutator_entropy_all_repairs.pdf")
ggsave(fn, plot = g, width = 10, height = 12)

#stop("")

# AGGREGATED RANKING
# ===
aggr = tbl %>%
  group_by(generator, mutator, D, eps, mu, repair) %>%
  dplyr::summarize(entropy_mean = mean(entropy)) %>%
  ungroup() %>%
  group_by(generator, D, eps, mu, repair) %>%
  dplyr::mutate(rank = rank(entropy_mean)) %>%
  ungroup()

table(aggr$mutator, aggr$rank, aggr$repair)


# EFFECT OF REPAIR
# ===
tbl2 = tbl %>% group_by(repair, mutator) %>%
  dplyr::summarise(
    no_improve = round(mean(iters_without_improvement / iters) * 100, digits = 2),
    did_repair = round(mean(repair_operations / (iters)) * 100, digits = 2)) %>%
  arrange(no_improve) %>%
  ungroup() %>%
  reshape2::melt(id.vars = c("repair", "mutator"), variable.name = "split", value.name = "value")

g = ggplot(tbl2, aes(x = mutator, y = value, fill = split))
g = g + geom_bar(stat = "identity", position = "dodge", alpha = 0.5)
g = g + theme_bw()
g = g + scale_fill_brewer(palette = "Dark2")
#g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45))
g = g + facet_grid(. ~ repair)
#print(g)
