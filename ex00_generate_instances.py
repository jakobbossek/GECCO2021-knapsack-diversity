from KP.knapsack import *
import pprint
import random

random.seed(1)
n = 100
for kptype in ["uncorr", "wcorr", "scorr", "ascorr", "invscorr", "ss", "usw"]:
  #kpi = generate(n = 100, R = 250, type = kptype)
  kpi = generate(n = 100, R = 10000, type = kptype)
  outfile = "study1/instances/" + str(kptype) + ".csv"
  kpi.save(outfile)

