import pprint
from itertools import product
import csv

n = 100
mutator = ["bf", "abf", "sbf", "bbf", "pbf", "htbf"]
crossover = ["on", "off"]
repair = ["off", "deterministic"]
generator = ["uncorr", "scorr", "invscorr", "usw"]
D = [2, 5, 10] # capacity (D/11) x <sum of weights>
mu = [25, 50, 100, 200]
eps = [0.1, 0.2, 0.5, 0.9]
repl = list(range(1, 11))

print("Generating experimental grid ...")
print("#experiments: {}".format(len(mutator) * len(crossover) * len(repair) * len(generator) * len(D) * len(mu) * len(eps) * len(list(repl))))

expfile = "study1/setup.csv"

with open(expfile, 'w', newline='') as f:
  writer = csv.writer(f, delimiter = " ")
  writer.writerow(["jobid", "mutator", "crossover", "repair", "generator", "D", "mu", "eps", "repl"])
  for id, experiment in enumerate(product(mutator, crossover, repair, generator, D, mu, eps, repl)):
    writer.writerow([id+1] + list(experiment))
