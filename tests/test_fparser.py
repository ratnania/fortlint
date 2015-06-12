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
def test_1(filename):
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
    parser.run(update_variables=True)

#    print (parser.text)

    if GRAPH:
        parser.graph_decl.render("graph-declarations.gv", view=False)
        parser.graph_call.render("graph-calls.gv", view=False)

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

#    test_1("fortran/test_1.F90")
#    test_1("fortran/test_2.F90")
#    test_1("fortran/test_3.F90")
#    test_1("fortran/test_4.F90")
#    test_1("fortran/test_5.F90")
    test_1("fortran/test_6.F90")

