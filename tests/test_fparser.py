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
def make_test(filename=None, dirname=None):
    from fortlint.coding_style import SubroutineBlockJorek
    from fortlint.coding_style import FunctionBlockJorek
    from fortlint.coding_style import ModuleBlockJorek

    dict_constructor = {}
    dict_constructor["module"]     = ModuleBlockJorek
    dict_constructor["subroutine"] = SubroutineBlockJorek
    dict_constructor["function"]   = FunctionBlockJorek

    dict_attribut    = {}
    dict_attribut["update_variables"] = True

    parser = Parser(filename=filename, \
                    dirname=dirname, \
                    dict_constructor=dict_constructor, \
                    dict_attribut=dict_attribut,
                    verbose=0)

    parser.run()

    if GRAPH:
        parser.graph_decl.render("graph-declarations.gv", view=False)
        parser.graph_call.render("graph-calls.gv", view=False)
#
#    filename_backup = filename+".BACKUP"
#    filename_out    = filename+".OUTPUT"
#
#    command = "cp " + filename + " " + filename_backup
#    os.system(command)
#
#    f = open(filename_out, 'w')
#    f.write(parser.text)
# ...

def test_1():
    filename = "fortran/test_1.F90"
    make_test(filename=filename)

def test_2():
    filename = "fortran/test_2.F90"
    make_test(filename=filename)

def test_3():
    filename = "fortran/test_3.F90"
    make_test(filename=filename)

def test_4():
    filename = "fortran/test_4.F90"
    make_test(filename=filename)

def test_5():
    filename = "fortran/test_5.F90"
    make_test(filename=filename)

def test_6():
    filename = "fortran/test_6.F90"
    make_test(filename=filename)

def test_7():
    filename = "fortran/test_7.F90"
    make_test(filename=filename)

def test_dirname():
    dirname = "fortran"
    make_test(dirname=dirname)

def test_input():
    import sys
    # -----------------------------------------------
    try:
        name = str(sys.argv[1])
    except:
        print ("you must provide an input file")
        raise()
    # -----------------------------------------------

    if os.path.isfile(name):
        make_test(filename=name)

    if os.path.isdir(name):
        make_test(dirname=name)

#############################################################################
if __name__ == "__main__":
#    test_1()
#    test_2()
#    test_3()
#    test_4()
#    test_5()
#    test_6()
#    test_7()
#    test_dirname()
    test_input()

