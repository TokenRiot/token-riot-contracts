#!/usr/bin/python
"""
Define test trees as list of predicate tuples. A predicate tuple has the form (statement, assertion, function).
"""
from TestSuite.predicate import t, f
from TestSuite.delay import force_block_change
from TestSuite.Tests.Reference.UpdateCashier.good_update_cashier_tx import good_update_cashier_tx

# List of all the tests for the tokenization and fractionalization
data_reference_tree = [
    ("Forced Block Delay", True, force_block_change), # update cashier information
    ("A correct tokenized transaction", "Transaction successfully submitted.", good_update_cashier_tx),
    ("Forced Block Delay", True, force_block_change), # update cashier information
]

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