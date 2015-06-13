# -*- coding: utf8 -*-

import re
import os
from fortlint.fparser import Parser
# ...
try:
    from graphviz import Digraph
    GRAPH=True
except ImportError:
    GRAPH=False
# ...

# ...
def make_test(filename):
    from fortlint.coding_style import SubroutineBlockJorek
    from fortlint.coding_style import FunctionBlockJorek
    from fortlint.coding_style import ModuleBlockJorek

    dict_constructor = {}
    dict_constructor["module"]     = ModuleBlockJorek
    dict_constructor["subroutine"] = SubroutineBlockJorek
    dict_constructor["function"]   = FunctionBlockJorek

    parser = Parser(filename=filename, dict_constructor=dict_constructor,
                    verbose=1)
    source = parser.text

    print (parser.dict_names)
#    parser.run(update_variables=False)
    parser.run(update_variables=True)

#    print (parser.text)

    if GRAPH:
        parser.graph_decl.render("graph-declarations.gv", view=False)
        parser.graph_call.render("graph-calls.gv", view=False)

    filename_backup = filename+".BACKUP"
#        filename_out    = filename
    filename_out    = filename+".OUTPUT"

    command = "cp " + filename + " " + filename_backup
    os.system(command)

    f = open(filename_out, 'w')
    f.write(parser.text)
# ...

def test_1():
    filename = "fortran/test_1.F90"
    make_test(filename)

def test_2():
    filename = "fortran/test_2.F90"
    make_test(filename)

def test_3():
    filename = "fortran/test_3.F90"
    make_test(filename)

def test_4():
    filename = "fortran/test_4.F90"
    make_test(filename)

def test_5():
    filename = "fortran/test_5.F90"
    make_test(filename)

def test_6():
    filename = "fortran/test_6.F90"
    make_test(filename)

def test_7():
    filename = "fortran/test_7.F90"
    make_test(filename)

def test_input():
    import sys
    # -----------------------------------------------
    try:
        filename = str(sys.argv[1])
    except:
        print ("you must provide an input file")
        raise()
    # -----------------------------------------------

    make_test(filename)

#############################################################################
if __name__ == "__main__":
#    test_1()
#    test_2()
#    test_3()
#    test_4()
#    test_5()
#    test_6()
#    test_7()
    test_input()

