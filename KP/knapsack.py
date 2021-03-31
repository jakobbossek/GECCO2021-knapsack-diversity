import random
import sys
import os
import csv
import math

class KnapsackInstance:
  '''
  Basic 0-1 Knapsack Object

  Args:
    capacity (int): Knapsack packing limit
    weights (list): List of integer weights
    profits (list): List of integer profits

  Attributtes:
    capacity (int): Knapsack capacity
    weights (list): List of integer weights
    profits (list): List of integer profits
    N             : Number of items
  '''
  def __init__(self, capacity, weights, profits):
    assert len(weights) == len(profits)
    #assert capacity >= 1

    self.weights = weights
    self.profits = profits
    self.capacity = capacity
    self.N = len(weights)

  def __str__(self):
    return "Capacity: {}\nItems: {}".format(self.capacity, self.N)

  def getItems(self):
    return zip(self.weights, self.profits)

  def getEfficiencies(self):
    return [self.profits[i] / self.weights[i] for i in range(self.N)]

  def evaluate(self, x):
    return self.wsum(x), self.psum(x)

  def wsum(self, x = None):
    if x is None:
      x = [1] * self.N
    return sum([w for i, w in enumerate(self.weights) if x[i] == 1])

  def wsumint(self, x):
    return sum([self.weights[i] for i in x])

  def psum(self, x = None):
    if x is None:
      x = [1] * self.N
    return sum([p for i, p in enumerate(self.profits) if x[i] == 1])

  def psumint(self, x):
    return sum([self.profits[i] for i in x])

  def to_bitstring(self, x):
    bs = [0] * self.N
    for i in x:
      bs[i] = 1
    return bs

  def scale_down_profits(self, eps):
    assert(eps > 0 and eps < 1)

    pmax = max(self.profits)
    N = self.N
    K = (eps * pmax) / N
    scaled_profits = [math.floor(profit / K) for profit in self.profits]
    # print(self.profits)
    # print(scaled_profits)
    # print("pmax: {}, K: {}, pmax_scaled: {}".format(pmax, K, max(scaled_profits)))
    return KnapsackInstance(self.capacity, self.weights, scaled_profits)

  def save(self, filepath):
    if os.path.exists(filepath):
      print("File '{}' already exists!".format(filepath))
      return False

    with open(filepath, "w") as f:
      writer = csv.writer(f, delimiter = " ")
      capacity = self.capacity
      if capacity is None:
        capacity = int(self.wsum() / 2)
      writer.writerow([capacity])

      # (weight, profit) pairs by row
      wp = list(map(list, zip(self.weights, self.profits)))
      writer.writerows(wp)

    return True


  @classmethod
  def load(cls, filepath):
    with open(filepath) as f:
      # first line is the knapsack limit
      capacity = int(next(f))
      # next lines are (w_i, p_i)-pairs
      wps = [tuple((int(word) for word in line.split())) for line in f]
      ws = [w for (w, _) in wps]
      ps = [p for (_, p) in wps]
      return cls(capacity, ws, ps)


def generate(n, R, type, L = 1):
  '''
  Knapsack problem generator

  See Section 3 in "Where are the hard Knapsack Problems?"
  by David Pisinger (http://www.dcs.gla.ac.uk/~pat/cpM/jchoco/knapsack/papers/hardInstances.pdf).

  Args:
    n (int): Number of items
    R (int): Upper limit for sampling of weights (and profits).
    type (str): Instances type; either uncorr (uncorrelated),
    wcorr (weakly correlated), scorr (strongly correlated),
    ascorr (almost strongly correlated), ss (subset sum),
    invscorr (inversely strongly correlated) or
    usw (uniform similar weights).
    L (int): Lower limit for sampling of weights. Default is 1.

  Returns:
    KnapsackInstance
  '''
  assert n >= 1
  assert R >= L
  assert type in ["uncorr", "wcorr", "scorr", "ascorr", "invscorr", "ss", "usw"]

  # sample weighs (same procedure for all types)
  ws = [random.randint(L, R) for _ in range(n)]

  R10 = round(R / 10)
  R500 = round(R / 500)

  if type == "uncorr":
    ps = [random.randint(L, R) for _ in range(n)]
  elif type == "wcorr":
    ps = [max(random.randint(w - R10, w + R10), 1) for w in ws]
  elif type == "scorr":
    ps = [w + R10 for w in ws]
  elif type == "ascorr":
    ps = [random.randint(w + R10 - R500, w + R10 + R500) for w in ws]
  elif type == "invscorr":
    ps = [random.randint(L, R) for _ in range(n)]
    ws = [p + R10 for p in ps]
  elif type == "ss":
    ps = [w for w in ws]
  elif type == "usw":
    ws = [random.randint(100000, 100100) for _ in range(n)]
    ps = [random.randint(1, 1000) for _ in range(n)]
  else:
    print("Unknown type {0}".format(type))
    sys.exit(1)

  return KnapsackInstance(None, ws, ps)

