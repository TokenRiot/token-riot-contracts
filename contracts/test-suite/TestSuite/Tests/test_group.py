#!/usr/bin/python
"""
Define test groups as list of test trees. A test group tuple has the form (statement, test_tree).
"""
from TestSuite.Tests.example_test_tree import example_tree, example_tree_with_failures
from TestSuite.Tests.Reference.reference_test_tree import reference_test_tree


# main test group
test_group = [
    ("Data Reference Contract Test Tree", reference_test_tree),
]

example_test_group = [
    ("Example test tree that succeeds", example_tree),
    ("Example test tree with failures", example_tree_with_failures),
]