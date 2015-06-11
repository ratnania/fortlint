# -*- coding: utf8 -*-

import re
import os

from fortlint.extractors import *


PRINT = False
PRINT = True


source_subroutine = \
"""
   SUBROUTINE    my_sub2 (xx,ii)
   IMPLICIT NONE
      REAL :: xx
      INTEGER :: ii

      ii = xx

      CALL my_other_sub(xx)

      RETURN
   END SUBROUTINE my_sub2
"""

source_function = \
"""
   FUNCTION    my_func3 (xx)
   IMPLICIT NONE
      REAL :: xx

      xx = xx + 1.0

      RETURN
   END FUNCTION my_func3
"""

source_module_1 = \
"""
   MODULE    my_module4
   IMPLICIT NONE
      REAL :: xx

      CONTAINS


   END MODULE my_module4
"""

source_module_2 = \
"""
   MODULE    my_module4
   IMPLICIT NONE
      REAL :: xx

      CONTAINS
""" \
        + source_function + \
"""
   END MODULE my_module4
"""

# ...
def test_extract_subroutine(source):
    keyword = "subroutine"
    _re = extract_blocks(keyword, "my_sub2")
    r = _re.findall(source.lower())
    if PRINT :
        print (">>> extract " + keyword)
        print (r)
        print ("")
# ...

# ...
def test_extract_function(source):
    keyword = "function"
    _re = extract_blocks(keyword, "my_func3")
    r = _re.findall(source.lower())
    if PRINT :
        print (">>> extract " + keyword)
        print (r)
        print ("")
# ...

# ...
def test_extract_module(source):
    keyword = "module"
    _re = extract_blocks(keyword, "my_module4")
    r = _re.findall(source.lower())
    if PRINT :
        print (">>> extract " + keyword)
        print (r)
        print ("")
# ...

# ...
def test_extract_subroutine_call(source):
    keyword = "subroutine"
    _re = extract_blocks(keyword, "my_sub2")
    r = _re.findall(source.lower())[0]

    _re = extract_subroutine_call()
    r = _re.findall(r)[0]

    if PRINT :
        print (">>> extract " + keyword)
        print (r)
        print ("")
# ...




#############################################################################
if __name__ == "__main__":
#    test_extract_subroutine(source_subroutine)
#    test_extract_function(source_function)
#    test_extract_module(source_module_1)
#    test_extract_module(source_module_2)
    test_extract_subroutine_call(source_subroutine)
