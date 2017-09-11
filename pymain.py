#!/usr/bin/env python3

def stateful(num):
  if not hasattr(stateful, 'counter'):
    stateful.counter = 0
  stateful.counter += num
  print(stateful.counter)
