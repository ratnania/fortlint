# -*- coding: utf8 -*-

import re
import os
from fortlint.fparser import Parser

# ...
def test_1(filename):
    from fortlint.coding_style import SubroutineBlockJorek
    from fortlint.coding_style import FunctionBlockJorek

    dict_constructor = {}
    dict_constructor["subroutine"] = SubroutineBlockJorek
    dict_constructor["function"]   = FunctionBlockJorek

    parser = Parser(filename=filename, dict_constructor=dict_constructor,
                    verbose=1)
    source = parser.text

    print (parser.dict_names)
    parser.run(update_variables=True)

    print (parser.text)

    filename_backup = filename+".BACKUP"
#        filename_out    = filename
    filename_out    = filename+".BACKUP"

    command = "cp " + filename + " " + filename_backup
    os.system(command)

    f = open(filename_out, 'w')
    f.write(parser.text)
# ...

#############################################################################
if __name__ == "__main__":

    test_1("fortran/test_1.F90")
    test_1("fortran/test_2.F90")

