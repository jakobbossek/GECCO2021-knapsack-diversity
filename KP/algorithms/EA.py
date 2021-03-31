from KP.knapsack import KnapsackInstance
from KP.utilities import *
import math
import time
import random
from numpy import random as numpyrandom
from numpy import linspace


class EASolution:
  '''
  (mu + 1) Evolutionary Diversity Optimization Evolutionary Algorithm (EDO-EA) solution object:

  Args:
    kpi (KnapsackInstance): Object of class KP.KnapsackInstance.KnapsackInstance
    capacity (int): knapsack capacity
    params (dict): parameters passed down to EDO-EA
  Returns:
    Object of
  '''
  pass


def calculate_absolute_frequencies(P):
  '''
  Given a set P of length n of binary vectors of length m returns the vector
  of absolute frequencies. It does so in time O(n*m)

  Args:
    P (list): population
  Returns:
    List of absolute frequencies
  '''
  mu =  len(P)
  n = len(P[0])
  hs = [0] * n
  for i in range(mu):
    for j in range(n):
      hs[j] += P[i][j]
  return hs

def update_absolute_frequencies(hs, drop, add):
  '''
  Updates absolute frequencies in time O(n)

  Args:
    hs (list): absolute frequencies
    drop (list): individual to drop
    add (list): individual to add
  Returns:
    Modified hs.
  '''
  n = len(drop)
  for i in range(n):
    hs[i] = hs[i] - drop[i] + add[i]
  return hs

def calculate_entropy_diversity_measure(hs, mu):
  '''
  Calculate entropy measure in O(n)

  Args:
    hs (list): list of absolute frequencies.
  Returns:
    Scalar float value.
  '''
  n = len(hs)
  H = 0
  for i in range(n):
    fi = hs[i] / mu
    # log(0) undefined
    if fi > 0:
      H += fi * math.log(fi)
  return (-1) * H

def repair_by_removing_items_in_decreasing_order_of_frequency(y, hs, kpi):
  return y

  # activate sampled zero items

def EDOEA(kpi, initial, eps, capacity = None, **kwargs):
  time_started = time.perf_counter()
  if capacity is None:
    capacity = kpi.capacity

  # variables for book-keeping
  iters = 0
  number_of_iters_without_improvement = 0
  number_of_repair_operations = 0
  total_number_of_iters_without_improvement = 0
  total_number_of_infeasible_offspring = 0
  total_number_of_repair_successes = 0

  n = kpi.N
  mu = kwargs["pop_size"]
  repair = kwargs.get("repair") # default is None
  mutator = kwargs.get("mutator")
  crossover = kwargs.get("crossover")
  pc = 0.8 # probability of crossover
  max_iters = kwargs.get("max_iters", n * mu)
  P = [initial] * mu

  hs = calculate_absolute_frequencies(P)
  item_indices_by_decreasing_hs = calculte_positions_in_sorted_order(hs, reverse = True)
  item_indices_by_increasing_hs = item_indices_by_decreasing_hs[:]
  item_indices_by_increasing_hs.reverse()
  item_indices_by_decreasing_efficiency = calculte_positions_in_sorted_order(kpi.getEfficiencies(), reverse = True)
  item_indices_by_increasing_efficiency = item_indices_by_decreasing_efficiency[:]
  item_indices_by_increasing_efficiency.reverse()
  entropy = calculate_entropy_diversity_measure(hs, mu)

  # print(hs)


  weights = [kpi.wsum(initial)] * mu
  profits = [kpi.psum(initial)] * mu
  pmin = (1 - eps) * profits[0]

  print("{} {:.6f} {} {} {}".format(iters, entropy, 0, 0, 0))

  # Evolutionary loop
  while (iters < max_iters):
    iters += 1
    improved_diversity = False

    # Sample individual for mutation
    idx = random.randint(0, mu - 1)
    y = P[idx][:] # copy
    yweight = None
    yprofit = None

    # TODO: rename stuff and refactor code
    if crossover is not None and (random.uniform(0, 1) < pc):
      y = crossover.crossover([P[random.randint(0, mu - 1)], P[random.randint(0, mu - 1)]])
      # TODO: copy and paste start
      yweight = kpi.wsum(y)
      yprofit = kpi.psum(y)

      #packing_is_infeasible = False
      packing_is_infeasible = yweight > kpi.capacity or yprofit < pmin
      # item violates capacity (profit cannot be violted since we only activated items)
      # NOTE: here we always repair
      if packing_is_infeasible:
        #number_of_repair_operations += 1
        #  repair by dropping items
        if yweight > kpi.capacity:
          if repair == "random":
            random.shuffle(item_indices_by_decreasing_hs)
          for i in item_indices_by_decreasing_hs:
            # was already active in parent?
            if y[i] == 1:
              yweight_without_i = yweight - kpi.weights[i]
              yprofit_without_i = yprofit - kpi.profits[i]
              # still above quality threshold
              if yprofit_without_i >= pmin:
                y[i] = 0
                yweight = yweight_without_i
                yprofit = yprofit_without_i
            # terminate if weight passes
            if yweight <= kpi.capacity:
              packing_is_infeasible = False
              break
        if yprofit < pmin:
          if repair == "random":
            random.shuffle(item_indices_by_increasing_hs)
          for i in item_indices_by_increasing_hs:
            # was inactive and is it still?
            if y[i] == 0:
              yweight_with_i = yweight + kpi.weights[i]
              yprofit_with_i = yprofit + kpi.profits[i]
              # still above quality threshold
              if yweight_with_i <= kpi.capacity:
                y[i] = 1
                yweight = yweight_with_i
                yprofit = yprofit_with_i
            # terminate if weight passes
            if yprofit >= pmin:
              packing_is_infeasible = False
              break
        # TODO: copy and paste end

    if mutator is not None:
      parent = y[:]
      y = mutator.mutate(parent, hs = hs)
      yweight = kpi.wsum(y)
      yprofit = kpi.psum(y)

      #packing_is_infeasible = False
      packing_is_infeasible = yweight > kpi.capacity or yprofit < pmin
      packing_was_infeasible_before_repair = yweight > kpi.capacity or yprofit < pmin
      # item violates capacity (profit cannot be violted since we only activated items)
      if packing_is_infeasible and repair != "off":# not None:
        number_of_repair_operations += 1
        #  repair by dropping items
        if yweight > kpi.capacity:
          if repair == "random":
            random.shuffle(item_indices_by_decreasing_hs)
          for i in item_indices_by_decreasing_hs:
            # was already active in parent?
            if parent[i] == 1 and y[i] == 1:
              yweight_without_i = yweight - kpi.weights[i]
              yprofit_without_i = yprofit - kpi.profits[i]
              # still above quality threshold
              if yprofit_without_i >= pmin:
                y[i] = 0
                yweight = yweight_without_i
                yprofit = yprofit_without_i
            # terminate if weight passes
            if yweight <= kpi.capacity:
              packing_is_infeasible = False
              break
        if yprofit < pmin:
          # TODO: here we should go through in incresing order frequency
          if repair == "random":
            random.shuffle(item_indices_by_increasing_hs)
          for i in item_indices_by_increasing_hs:
            # was inactive and is it still?
            if parent[i] == 0 and y[i] == 0:
              yweight_with_i = yweight + kpi.weights[i]
              yprofit_with_i = yprofit + kpi.profits[i]
              # still above quality threshold
              if yweight_with_i <= kpi.capacity:
                y[i] = 1
                yweight = yweight_with_i
                yprofit = yprofit_with_i
            # terminate if weight passes
            if yprofit >= pmin:
              packing_is_infeasible = False
              break

    # now drop item with least entropy contribution
    # NOTE: all individuals are valid now
    if packing_is_infeasible:
      total_number_of_infeasible_offspring += 1

    if not packing_is_infeasible:
      if packing_was_infeasible_before_repair:
        total_number_of_repair_successes += 1
      drop_for_best_diversity = mu #  i.e., drop y
      best_entropy = entropy
      for i in range(mu):
        # update (pass by reference)
        update_absolute_frequencies(hs, drop = P[i], add = y)
        entropy2 = calculate_entropy_diversity_measure(hs, mu)
        update_absolute_frequencies(hs, drop = y, add = P[i])
        if entropy2 >= best_entropy:
          drop_for_best_diversity = i
          best_entropy = entropy2

      # found item to be replaced is not the newly generated offspring -> replace
      if drop_for_best_diversity != mu:
        if best_entropy > entropy:
          improved_diversity = True
        update_absolute_frequencies(hs, drop = P[drop_for_best_diversity], add = y)
        P[drop_for_best_diversity] = y
        weights[drop_for_best_diversity] = yweight
        profits[drop_for_best_diversity] = yprofit
        entropy = best_entropy

      item_indices_by_decreasing_hs = calculte_positions_in_sorted_order(hs, reverse = True)
      item_indices_by_increasing_hs = item_indices_by_decreasing_hs[:]
      item_indices_by_increasing_hs.reverse()

    if not improved_diversity:
      number_of_iters_without_improvement += 1
      total_number_of_iters_without_improvement += 1
    else:
      number_of_iters_without_improvement = 0

    assert(len([i for i in range(n) if hs[i] < 0 or hs[i] > mu]) == 0)

    print("{} {:.6f} {} {} {}".format(iters, entropy, int(improved_diversity), int(packing_is_infeasible), int(packing_was_infeasible_before_repair and (not packing_is_infeasible))))

  #print(hs)
  assert(len([kpi.wsum(x) <= kpi.capacity for x in P]) == len(P))
  assert(len([kpi.psum(x) >= pmin for x in P]) == len(P))
  assert(all([hs[i] >= 0 and hs[i] <= mu for i in range(n)]))
  #print(hs)
  #print(calculate_entropy_diversity_measure(hs, mu))

  # measure total time
  time_passed = math.ceil(time.perf_counter() - time_started)

  print(' '.join(map(str, [iters, time_passed, entropy, total_number_of_iters_without_improvement, number_of_repair_operations, total_number_of_repair_successes])))

  return {"population": P, "entropy": entropy, "kpi": kpi, "time_passed": time_passed, "total_number_of_iters_without_improvement": total_number_of_iters_without_improvement, "number_of_repair_operations": number_of_repair_operations, "total_number_of_repair_successes": total_number_of_repair_successes, "ea_args": kwargs}



# class DPWBSolution:
#   '''
#   Dynamic Programming Weight-Based (WB) approach solution object:

#   Args:
#     kpi (KnapsackInstance): Object of class KP.KnapsackInstance.KnapsackInstance
#     capactiy (int): Knapsack capactiy
#     profits_table: table m(i, j) which is the maximum profit reachable
#     with items {1,...,i} and maximum weight j.
#     nsols_table: entry c(i, j) indicates the number of solutions with maximum profit m(i,j) which can be reached by items {1,...,i} and maximum wieight j.
#   Returns:
#     Object of type DPWBSolution
#   '''
#   def __init__(self, kpi, capacity, profits_table, nsols_table):
#     self.kpi = kpi
#     self.profits_table = profits_table
#     self.nsols_table = nsols_table
#     self.N = kpi.N
#     self.capacity = capacity

#   def n_optima(self):
#     return self.nsols_table[self.N][self.capacity]

#   def optima_single(self):
#     # We want just a single global optimum
#     reconstruction = []
#     i = self.kpi.N
#     j = self.capacity
#     items = list(self.kpi.getItems())

#     while i > 0:
#       # traceback in the table starting from tbl[N, capacity]
#       if self.profits_table[i][j] != self.profits_table[i - 1][j]:
#         reconstruction.append(i - 1)
#         j -= items[i - 1][0]
#       i -= 1

#     return reconstruction

#   def optima_all(self):
#     # We want ALL global optima
#     reconstruction = []
#     i = self.kpi.N
#     j = self.capacity
#     items = list(self.kpi.getItems())

#     count = 0
#     # we store partially reconstructed packing plans in a queue
#     Q = queue.Queue()
#     Q.put((self.kpi.N, self.capacity, []))
#     while not Q.empty():
#       i, j, packing = Q.get()
#       # classic reconstruction starts
#       while i > 0:
#         if self.profits_table[i][j] != self.profits_table[i - 1][j]:
#           # item was packed, i.e. self.profits_table[i-1][j] < self.profits_table[i, j]
#           packing.append(i - 1)
#           j -= items[i - 1][0]
#         elif (j - items[i - 1][0] >= 0) and (self.profits_table[i][j] == (self.profits_table[i - 1][j - items[i-1][0]] + items[i-1][1])):
#           # not packing item results in the same profit as with items {0, ...,  n-2}?
#           partialPacking = packing[:]  # make copy
#           partialPacking.append(i - 1)
#           Q.put((i - 1, j - items[i - 1][0], partialPacking))
#         i -= 1
#       count += 1
#       reconstruction.append(packing)

#     return reconstruction

#   def solutions_sample(self, k, replace=True):
#     assert 1 <= k
#     pass


# def DPWB(kpi, capacity=None):#, multiGlobal=True, countOnly=False):
#   '''
#   Dynamic Programming algorithm for the 0-1 Knapsack Problem (KP)

#   Args:
#     kpi (KnapsackInstance): Object of class KP.KnapsackInstance.KnapsackInstance
#     capacity (int)        : Knapsack capacity, i.e. maximum weight of packed items. Defaults
#     to the capacity of KI if None.
#   Returns:
#     An object of class DPSolution
#   '''
#   if capacity is None:
#     capacity = kpi.capacity

#   # (w_i, p_i)
#   items = list(kpi.getItems())

#   # init table
#   tbl = [[0] * (capacity + 1) for _ in range(kpi.N + 1)]
#   nsols = [[1] * (capacity + 1) for _ in range(kpi.N + 1)]

#   for i, (weight, profit) in enumerate(items):
#     i += 1
#     for cap in range(capacity + 1):
#       if (weight > cap):
#         # (i-1)-th element does not fit in
#         tbl[i][cap] = tbl[i - 1][cap]
#         nsols[i][cap] = nsols[i - 1][cap]
#       else:
#         # otherwise check if it is of benefit to pack the item or not
#         packOptionA = tbl[i - 1][cap]
#         packOptionB = tbl[i - 1][cap - weight] + profit

#         if (packOptionA == packOptionB):
#           tbl[i][cap] = packOptionA
#           nsols[i][cap] = nsols[i - 1][cap] + nsols[i - 1][cap - weight]
#         elif (packOptionA > packOptionB):
#           tbl[i][cap] = packOptionA
#           nsols[i][cap] = nsols[i - 1][cap]
#         else:
#           tbl[i][cap] = packOptionB
#           nsols[i][cap] = nsols[i - 1][cap - weight]

#   return DPWBSolution(kpi=kpi, capacity=capacity, profits_table=tbl, nsols_table=nsols)

