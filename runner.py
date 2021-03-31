from KP.knapsack import *
from KP.algorithms.DP import *
from KP.algorithms.EA import *
from KP.algorithms.EA_operators import *
from itertools import product
import numpy
import pprint
import csv
import sys

def read_setup(setupfile, jobid):
  '''
  Read setup parameters from file.
  jobid is the line number. Additionally the first line is read. It contains the
  parameter names. Return value is a dict with the names as keys and the string
  parameters as values.
  '''
  with open(setupfile, 'r') as f:
    lines = [x for i, x in enumerate(f) if i in [0, int(jobid)]]
    lines = [line.strip("\n").split(' ') for line in lines]
    assert(len(lines) == 2)
    return dict(zip(lines[0], lines[1]))

def get_mutator(mstring, n, mu):
  if mstring == "bf":
    return UniformBitflipMutator(n)
  elif mstring == "pbf":
    return PoissonBitflipMutator(n)
  elif mstring == "htbf":
    return HeavyTailedBitflipMutator(n)
  elif mstring == "bbf":
    return KnapsackDiversityBiasedBitflipMutator(n)
  elif mstring == "abf":
    return KnapsackDiversityAsymmetricBitflipMutator(n, mu = mu)
  elif mstring == "sbf":
    return KnapsackDiversitySymmetricBitflipMutator(n, mu = mu)

def get_crossover(cstring, n, mu):
  if cstring == "on":
    return CrossoverWithEfficiencyFillUp(n)
  return None

def run(jobid):
  rngseed = int(jobid)
  setup = read_setup("study1/setup.csv", jobid)
  mutator = setup['mutator']
  crossover = setup['crossover']
  repair = setup['repair']
  generator = setup['generator']
  eps = float(setup['eps'])
  mu = int(setup['mu'])
  D = int(setup['D'])
  kpi = KnapsackInstance.load("study1/instances/" + generator + ".csv")
  capacity = (D / 11) * kpi.wsum()

  approx = DPPB(kpi, capacity = capacity, eps = eps).optima_single()
  initial = kpi.to_bitstring(approx)
  n = len(initial)
  #print(setup)

  random.seed(rngseed)
  numpy.random.seed(rngseed)
  #print("generator: {}, n: {}, D: {}, mu: {}, eps: {}, capacity: {}, repair: {}".format(generator, n, D, mu, eps, capacity, repair))
  res = EDOEA(kpi, initial, eps, capacity, pop_size = mu, max_iters = mu * n, mutator = get_mutator(mutator, n, mu), crossover = get_crossover(crossover, n, mu), repair = repair)

# pass first command line argument (the jobid)
run(sys.argv[1])
