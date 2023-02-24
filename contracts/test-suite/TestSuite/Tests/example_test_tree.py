#!/usr/bin/python
"""
Define test trees as list of predicate tuples. A predicate tuple has the form (statement, assertion, function).
"""
from TestSuite.predicate import t, f
from TestSuite.delay import force_block_change

# example test trees with failures
example_tree_with_failures = [
    ("Testing if the True function is True",   True,  t),
    ("Testing if the False function is False", False, f),
    ("Testing if the False function is true",  True,  f),
    ("Testing if the True function is False",  False, t),
]

# example tree with only successes
example_tree = [
    ("Testing if the True function is True",   True,  t),
    ("Testing if the False function is False", False, f),
]