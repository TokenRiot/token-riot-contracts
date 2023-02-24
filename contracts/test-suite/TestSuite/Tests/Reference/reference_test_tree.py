#!/usr/bin/python
"""
Define test trees as list of predicate tuples. A predicate tuple has the form (statement, assertion, function).
"""
from TestSuite.predicate import t, f
from TestSuite.delay import force_block_change
from TestSuite.Tests.Reference.UpdateCashier.good_update_cashier_tx import good_update_cashier_tx

# List of all the tests for the tokenization and fractionalization
reference_test_tree = [
    ("Forced Block Delay", True, force_block_change), # update cashier information
    ("A correct tokenized transaction", "Transaction successfully submitted.", good_update_cashier_tx),
    ("Forced Block Delay", True, force_block_change), # update cashier information
]
