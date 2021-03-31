from KP.knapsack import KnapsackInstance
import queue
import math

# PROFIT-BASED-APPROACH
# ===

class DPPBSolution:
  def __init__(self, kpi, capacity, profits_table, eps):
    self.kpi = kpi
    self.profits_table = profits_table
    self.N = kpi.N
    self.capacity = capacity
    self.profit_limit = self.N * max(kpi.profits)
    self.eps = eps

  def optima_single(self):
    # We want just a single global optimum
    reconstruction = []
    i = self.kpi.N
    j = self.profit_limit
    items = list(self.kpi.getItems())

    # determine maximal profit j such that profits_table[N, j] <= W
    j = max([k for k in range(self.profit_limit + 1) if self.profits_table[self.N][k] <= self.capacity])

    while i > 0:
      # traceback in the table starting from tbl[N, capacity]
      if self.profits_table[i][j] != self.profits_table[i - 1][j]:
        reconstruction.append(i - 1)
        j -= items[i - 1][1]  # subtract profit
      i -= 1

    return reconstruction


def DPPB(kpi, capacity = None, eps = None):
  '''
  Dynamic Programming algorithm for the 0-1 Knapsack Problem (KP)

  Args:
    kpi (KnapsackInstance): Object of class KP.KnapsackInstance.KnapsackInstance
    capacity (int)        : Knapsack capacity, i.e. maximum weight of packed items. Defaults
    to the capacity of KI if None.
    eps (float): Number between (0,1) for FPTAS.
  Returns:
    An object of class DPSolution
  '''
  if capacity is None:
    capacity = kpi.capacity

  if eps is not None:
    assert(eps > 0 and eps < 1)
    kpi = kpi.scale_down_profits(eps)

  # (w_i, p_i)
  items = list(kpi.getItems())

  # maximum profif
  N = kpi.N
  P = max(kpi.profits)  # TODO: for 0-1 KP sum(kpi.profits) should also be fine
  profit_limit = N * P

  # init table(s)
  # Each row: [0, Inf, Inf, ..., Inf]
  tbl = [[0] + [math.inf] * (profit_limit) for _ in range(N + 1)]
  nsols = [[1] + [0] * (profit_limit + 1) for _ in range(N + 1)]

  for i, (weight, profit) in enumerate(items):
    i += 1
    for p in range(profit_limit + 1):
      if (profit > p):
        # (i-1)-th element does not fit in
        tbl[i][p] = tbl[i - 1][p]
      else:
        # otherwise check if it is of benefit to pack the item or not
        packOptionA = tbl[i - 1][p]
        packOptionB = tbl[i - 1][p - profit] + weight

        if (packOptionA == packOptionB):
          tbl[i][p] = packOptionA
        elif (packOptionA < packOptionB):
          tbl[i][p] = packOptionA
        else:
          tbl[i][p] = packOptionB

  return DPPBSolution(kpi = kpi, capacity = capacity, profits_table = tbl, eps = eps)
