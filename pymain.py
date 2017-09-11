#!/usr/bin/env python3

def stateful(i):
  """ Count the total sum of passed in arguments between all calls to this
  function. """
  if not hasattr(stateful, 'counter'):
    stateful.counter = 0
  stateful.counter += i
  print('Python: ' + str(stateful.counter))
  return stateful.counter
