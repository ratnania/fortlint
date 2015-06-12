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
      CALL my_func(xx, xx)

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
      REAL :: alpha

      s = my_func(s)
      CALL my_sub33()

      CONTAINS
""" \
        + source_function + \
"""
   END MODULE my_module4
"""

f = open("fortran/test_6.F90", "r")
source_test_6 = f.read()

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

# ...
def test_extract_module_contains(source):
    keyword = "module"
    _re = extract_blocks(keyword, "my_module4")
    r = _re.findall(source.lower())[0]

    _re = extract_contains()
    r = _re.findall(r)[0]

    if PRINT :
        print (">>> extract " + keyword)
        print (r)
        print ("")
# ...

# ...
def test_extract_module_contains2(source):
    keyword = "module"
    _re = extract_blocks(keyword, "my_mod_1")
    source = _re.findall(source.lower())[0]

    dict_decl, dict_call = get_declarations_calls(source)

    if PRINT :
        print (">>> extract " + keyword)
        print (">>> dict_decl :", dict_decl)
        print (">>> dict_call :", dict_call)
        print ("")
# ...




#############################################################################
if __name__ == "__main__":
#    test_extract_subroutine(source_subroutine)
#    test_extract_function(source_function)
#    test_extract_module(source_module_1)
#    test_extract_module(source_module_2)
#    test_extract_subroutine_call(source_subroutine)
#    test_extract_module_contains(source_module_2)
    test_extract_module_contains2(source_test_6)
