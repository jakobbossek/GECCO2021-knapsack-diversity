import random
from numpy import random as numpyrandom, linspace, log1p
from numpy.random import rand

__all__ = ["BaseMutator", "UniformBitflipMutator", "RandomizedLocalSearchBitflipMutator", "PoissonBitflipMutator", "HeavyTailedBitflipMutator", "KnapsackDiversityBiasedBitflipMutator", "KnapsackDiversityAsymmetricBitflipMutator", "KnapsackDiversitySymmetricBitflipMutator", "CrossoverWithEfficiencyFillUp"]

def sample(cdf):
    """
    Samples a random positive bounded integer according to the Cumulative Distribution Function
  given as input (under the form of an array 'cdf', with cdf[i + 1] >= cdf[i]
    and cdf[-1] = 1. Note that these verifications are not performed by the function).

    The resulting integer X has the property that
        Pr(X <= i + 1) = cdf[i], for i in [0, ..., n - 1] :

    Pr(X = 1) = cdf[0]
    Pr(X = 2) = cdf[1] - cdf[0]
    ...
    Pr(X = n) = cdf[n - 1] - cdf[n - 2] - ... - cdf[0]

    """
    n = len(cdf)
    x = rand()
    if x < cdf[0]:
        return 1
    low = 0
    high = n - 1
    while high - low > 1:
        mid = (low + high) >> 1  # same as (low + high) // 2, but hopefully (mildly) faster
        if x > cdf[mid]:
            low = mid
        else:
            high = mid
    return high + 1


def sample_waiting_time(p):
    """
    Samples the number of independent tries until a given random event happens.
    'p' should be the probability of the event happening in one try.
    """
    x = rand()
    # We use log1p(z) to compute precisely log(1 + z) :
    return 1 + int(log1p(- x) // log1p(- p))


class BaseCrossover:
  def __init__(self, n):
    self.n = n

  def crossover(self, parents, **kwargs):
    raise NotImplementedError()


class CrossoverWithEfficiencyFillUp(BaseCrossover):
  def __init__(self, n):
    super().__init__(n)

  def crossover(self, parents, **kwargs):
    p1, p2 = parents[0], parents[1]
    n = self.n
    ch = [0] * n
    # keep common items
    for i in range(n):
      if p1[i] == 1 and p2[i] == 1:
        ch[i] = 1
    return ch

class BaseMutator:
  def __init__(self, n):
    self.n = n

  def mutate(self, x, in_place = False, **kwargs):
    n = self.n

    if not in_place:
      x = x.copy()

    for i in self.sample(x, **kwargs):
      x[i] = 1 - x[i]

    #print("k = {}".format(sum(x)))

    if not in_place:
      return x

  def sample(self, x, **kwargs):
    raise NotImplementedError()

class UniformBitflipMutator(BaseMutator):
  def __init__(self, n, theta = 1):
    super().__init__(n)
    self.theta = theta

  def sample(self, x, **kwargs):
    rate = self.theta / self.n
    for i in range(self.n):
      if random.random() < rate:
        yield i

class RandomizedLocalSearchBitflipMutator(BaseMutator):
  def __init__(self, n):
    super().__init__(n)

  def sample(self, x, **kwargs):
    return [random.randint(0, self.n - 1)]

class PoissonBitflipMutator(BaseMutator):
  def __init__(self, n, lam = 1, offset = 1):
    super().__init__(n)
    self.lam = lam
    self.offset = offset

  def sample(self, x, **kwargs):
    k = numpyrandom.poisson(lam = self.lam) + self.offset
    k = min(k, self.n)
    #print("k = {}".format(k))
    return random.sample(range(self.n), k = k)

class HeavyTailedBitflipMutator(BaseMutator):
  def __init__(self, n, beta = 1.5):
    super().__init__(n)
    self.beta = beta

    # copied from https://github.com/FastGA/fast-genetic-algorithms.git
    power_law = (linspace(1, n // 2, n // 2) ** (-beta)).cumsum()
    power_law /= power_law[-1]
    self.power_law = power_law

  def sample(self, x, **kwargs):
    rate = sample(self.power_law) / self.n
    index = sample_waiting_time(rate) - 1  # We need to subtract 1 to get a chance
    # to mutate the first index.
    while index < self.n:
      yield index
      index += sample_waiting_time(rate)

class KnapsackDiversityBiasedBitflipMutator(BaseMutator):
  def __init__(self, n):
    super().__init__(n)

  def sample(self, x, **kwargs):
    n = self.n

    # zero and one item indizes
    I0 = [i for i in range(n) if x[i] == 0]
    # I1 = [i for i in range(n) if x[i] == 1]

    # sample random number
    k = 1 + numpyrandom.poisson(lam = 1)
    nI0 = len(I0)
    k = min(k, nI0)

    # sample random zero bits without replacement
    return random.sample(I0, k = k)

class KnapsackDiversityAsymmetricBitflipMutator(BaseMutator):
  def __init__(self, n, mu):
    super().__init__(n)
    self.mu = mu

  def sample(self, x, **kwargs):
    n = self.n
    mu = self.mu
    hs = kwargs["hs"]

    for i in range(n):
      if x[i] == 0 and hs[i] <= (mu / 2):
        if random.random() <= (mu - hs[i]) / (2 * n) + (1 / n):
          yield i
      elif x[i] == 1 and hs[i] > (mu / 2):
        # NOTE: this is > 1/2 if hs[i] == mu, i.e., in the beginning and mu >= n
        if random.random() <= (hs[i]) / (2 * n) + (1 / n):
          yield i
      else:
        if random.random() <= (1 / n):
          yield i

# RENAME :)
class KnapsackDiversitySymmetricBitflipMutator(BaseMutator):
  def __init__(self, n, mu):
    super().__init__(n)
    self.mu = mu

  def sample(self, x, **kwargs):
    n = self.n
    mu = self.mu
    hs = kwargs["hs"]

    I0 = [i for i in range(n) if x[i] == 0]
    I1 = [i for i in range(n) if x[i] == 1]

    # TODO: bias towards lower frequency
    #I0_probs = [hs[i] / mu for ]

    k0 = 1 + numpyrandom.poisson(lam = 1)
    k1 = 1 + numpyrandom.poisson(lam = 1)

    # take minimum to avoid errors if there are less than k such bits
    return random.sample(I0, k = min(k0, len(I0))) + random.sample(I1, k = min(k1, len(I1)))

if __name__ == "__main__":
  x = [0] * 10
  BitFlip = UniformBitflipMutator(len(x), theta = 10)
  BitFlip = RandomizedLocalSearchBitflipMutator(len(x))
  BitFlip = PoissonBitflipMutator(len(x))
  BitFlip = HeavyTailedBitflipMutator(len(x), beta = 1.5)
  BitFlip = KnapsackDiversityBiasedBitflipMutator(len(x))
  BitFlip = KnapsackDiversityAsymmetricBitflipMutator(len(x), mu = 4)
  BitFlip = KnapsackDiversitySymmetricBitflipMutator(len(x), mu = 4)

  print(x)
  hs = [3, 3, 3, 2, 2, 1, 0, 0, 1, 0]
  print("===")
  for i in range(10):
    print(BitFlip.mutate(x, hs = hs))

