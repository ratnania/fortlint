# -*- coding: utf8 -*-

from constants import PREFIX_DECLARATION_TYPE_INTEGER
from constants import PREFIX_DECLARATION_TYPE_REAL
from constants import PREFIX_DECLARATION_TYPE_CHARACTER
from constants import PREFIX_DECLARATION_TYPE_LOGICAL
from constants import PREFIX_CONTEXT_TYPE_LOCAL
from constants import PREFIX_CONTEXT_TYPE_ARGUMENT
from constants import PREFIX_CONTEXT_TYPE_MODULE
from constants import PREFIX_CONTEXT_TYPE_OBJECT
from constants import PREFIX_CONTEXT_TYPE_GLOBAL
from extractors import *
from graph import Digraph
import re
import os

# ...
class Parser(object):
    """
    Python Class for Fortran language parser
    """
    def __init__(self, filename=None, dirname=None, \
                dict_constructor=None, \
                verbose=0):

        self._filename         = filename
        self._dirname          = dirname
        self._text             = None
        self._dict_names       = {}
        self._dict_constructor = dict_constructor
        self._verbose          = verbose
        self._graph            = Digraph(comment="Fortran Parser Graph")

        if filename is not None:
            f = open(filename, 'r')
            progtext = f.read()
            self._text = progtext.lower()
            f.close()

            self._dict_names['subroutine'] = get_names_subroutine(self.text)
            self._dict_names['function']   = get_names_function(self.text)

    @property
    def filename(self):
        return self._filename

    @property
    def dirname(self):
        return self._dirname

    @property
    def text(self):
        return self._text

    @property
    def dict_names(self):
        return self._dict_names

    @property
    def dict_constructor(self):
        return self._dict_constructor

    @property
    def verbose(self):
        return self._verbose

    @property
    def graph(self):
        return self._graph

    # ...
    def run(self, update_variables=True):
        source = self.text

        if self.verbose > 0:
            print (self.dict_names)

        for key, values in self.dict_names.items():
            keyword = key

            for block_name in values:
                is_modified = False

                if self.verbose > 0:
                    print (">>>>> block_name:" + block_name)

                constructor = self.dict_constructor[keyword]
                block = constructor(TAG=block_name, source=source)
                block.get_code()
                block.get_signature()
                block.get_arguments()
                block.get_sons()
                block.parse_variables()
                if update_variables:
                    for var in block.variables:
                        block.replace_variable(var)
                    is_modified = True

                if is_modified:
                    block.update_source()

                # ... update Graph
                block.update_graph(self._graph)
                # ...

                source = block.source

        self._text = source
# ...

