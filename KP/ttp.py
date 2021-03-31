import re
from .knapsack import KnapsackInstance

class TTPInstance:
  def __init__(self, name, ttptype, n, m, capacity, vmin, vmax, R, coordinates, items):
    assert n >= 2
    assert m >= (n - 1)
    assert len(coordinates) == n
    assert len(items) == m

    self.name = name
    self.ttptype = ttptype
    self.n = n
    self.m = m
    self.capacity = capacity
    self.vmin = vmin
    self.vmax = vmax
    self.R = R
    self.coordinates = coordinates
    # (profit, weight, assigned node number)
    self.items = items
    pass

  def toKnapsackInstance(self):
    weights = [w for [_, w, _] in self.items]
    profits = [p for [p, _, _] in self.items]
    return KnapsackInstance(capacity = self.capacity, weights = weights, profits = profits)

  @classmethod
  def load(self, filepath):
    def parseLine(line):
      # get. just the value
      line = line.split(":")[-1]
      return re.sub("\s+", "", line)

    with open(filepath, 'r', encoding = "utf-8") as f:
      # header
      name = parseLine(next(f))
      ttptype = {"boundedstronglycorr" : "bsc", "uncorrelated": "uncorr", "uniformsimilarweights": "usw"}[parseLine(next(f))]
      n = int(parseLine(next(f)))
      m = int(parseLine(next(f)))
      capacity = int(parseLine(next(f)))
      vmin = float(parseLine(next(f)))
      vmax = float(parseLine(next(f)))
      R = float(parseLine(next(f)))

      # skip next to lines
      next(f)
      next(f)

      # Coordinates
      coordinates = [list(map(int, next(f).split()[1:])) for _ in range(n)]

      # items
      next(f)
      # (profit, weight, assigned node number)
      items = [list(map(int, next(f).split()[1:])) for _ in range(m)]
      return TTPInstance(name = name, ttptype = ttptype, n = n, m = m, capacity = capacity, vmin = vmin, vmax = vmax, R = R, coordinates = coordinates, items = items)
