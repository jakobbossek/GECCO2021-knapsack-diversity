'''
Utiliy functions.
'''

__all__ = ["counting_sort", "counting_sorted", "calculte_positions_in_sorted_order"]

def counting_sort(A, k, reverse = False):
  # C[i] = c if i accurs at c positions in x
  C = [0] * (k + 1)
  n = len(A)

  # do the counting
  for i in range(n):
    C[A[i]] += 1

  # Now do the sorting
  iter_range = range(k, -1, -1) if reverse else range(k + 1)
  j = 0
  for i in iter_range:
    K = C[i]
    while K > 0:
      A[j] = i
      j += 1
      K -= 1

def counting_sorted(A, k, reverse = False):
  B = A[:]
  counting_sort(B, k, reverse)
  return B


def calculte_positions_in_sorted_order(A, reverse = False):
  '''
  Calculate ranks for list, i.e., the positions of the items in the sorted order
  TODO: this is O(n log n). Do this in O(n)!
  '''
  return [i for (i, _) in sorted(enumerate(A), key = lambda e: e[1], reverse = reverse)]
