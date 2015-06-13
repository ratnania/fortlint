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
        self._graph_decl       = Digraph(name="declarations")
        self._graph_call       = Digraph(name="calls")
        self._list_block       = []

        if filename is not None:
            f = open(filename, 'r')
            progtext = f.read()
            self._text = progtext.lower()
            f.close()

            self._dict_names['module']     = get_names_module(self.text)
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
    def graph_decl(self):
        return self._graph_decl

    @property
    def graph_call(self):
        return self._graph_call

    @property
    def blocks(self):
        return self._list_block

    def append(self, block):
        self._list_block.append(block)

    def get_block_index(self, block):
        return self.blocks.index(block)

    def get_block_by_id(self, ID):
        for b in self.blocks:
            if id(b) == ID:
                return b
        return None

    def get_block_by_name(self, name):
        for b in self.blocks:
            if b.name == name:
                return b
        return None

    def get_block_by_label(self, label):
        for b in self.blocks:
            if b.label == label:
                return b
        return None

    # ...
    def run(self, update_variables=True):
        source = self.text

        if self.verbose > 0:
            print (self.dict_names)

        # ... parse, replace,
        for key, values in self.dict_names.items():
            keyword = key

            for block_name in values:
                is_modified = False

                if self.verbose > 0:
                    print (">>>>> block_name:" + block_name)

                if len(block_name) > 0:
                    constructor = self.dict_constructor[keyword]
                    block = constructor(TAG=block_name, source=source)
                    block.get_code()
#                    print "------------------------"
#                    print block.source
#                    print "------------------------"
                    if block.is_valid:
                        block.get_signature()
                        block.get_arguments()
                        block.get_decl_call()
                        block.parse_variables()
                        if update_variables:
                            for var in block.variables:
                                block.replace_variable(var)
                            is_modified = True

                        if is_modified:
                            block.update_source()

                        # ... append block in the blocks list
                        self.append(block)
                        # ...

                source = block.source
        self._text = source
        # ...

        # ... update labels
        for block in self.blocks:
            block.update_label(self)
        # ...

        # ... update Graph
        for block in self.blocks:
            block.update_graph_decl(self)
            block.update_graph_call(self)
        # ...
# ...

