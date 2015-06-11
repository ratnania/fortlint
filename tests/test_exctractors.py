# -*- coding: utf8 -*-

import re
import os

from fortlint.extractors import *

# ...
def test_extract_subroutine():
    source = """
                SUBROUTINE    my_sub2 (xx,ii)
                IMPLICIT NONE
                   REAL :: xx
                   INTEGER :: ii

                   ii = xx

                   RETURN
                END SUBROUTINE my_sub2
              """

    _re = extract_blocks("subroutine", "my_sub2")
    print _re.findall(source.lower())
# ...


#############################################################################
if __name__ == "__main__":
    test_extract_subroutine()
