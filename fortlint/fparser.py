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
from constants import FORTRAN_EXTENSIONS
from extractors import *
from graph import Digraph
import re
import os
import sys
from glob import glob

# ...
class Parser(object):
    """
    Python Class for Fortran language parser
    """
    def __init__(self, \
                 filename=None, \
                 dirname=None, \
                 text=None, \
                 dict_constructor=None, \
                 dict_attribut=None, \
                 verbose=0):

        self._dirname          = dirname
        self._dict_names       = {}
        self._dict_constructor = dict_constructor
        self._dict_attribut    = dict_attribut
        self._verbose          = verbose
        self._graph_decl       = Digraph(name="declarations")
        self._graph_call       = Digraph(name="calls")
        self._list_block       = []
        self._prefix           = None
        # ... contains the source code for each filename
        self._dict_text        = {}

        if (filename is not None) and (dirname is not None):
            print ("Parser cannot be called with both filename and dirname not empty")
            raise()

        if filename is not None:
            f = open(filename, 'r')
            progtext = f.read()
            self._dict_text[filename] = progtext.lower()
            f.close()

        if dirname is not None:
            for root, subdirs, files in os.walk(dirname):
                print "++ searching in ", root
                if files is not None:
                    for File in files:
                        ext = File.split(".")[-1]
                        if ext in FORTRAN_EXTENSIONS:
                            _filename =  os.path.join(root, File)

                            print "---- filename :", _filename

                            f = open(_filename, "r")
                            progtext = f.read()
                            self._dict_text[_filename] = progtext.lower()
                            f.close()


        # ... init dict names
        self._dict_names['module']     = []
        self._dict_names['subroutine'] = []
        self._dict_names['function']   = []
        # ...

    @property
    def dirname(self):
        return self._dirname

    @property
    def dict_names(self):
        return self._dict_names

    @property
    def dict_constructor(self):
        return self._dict_constructor

    @property
    def dict_attribut(self):
        return self._dict_attribut

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

    @property
    def dict_text(self):
        return self._dict_text

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
    def run(self):
        i = 0
        for filename, text in self.dict_text.items():
            try:
                for name in get_names_module(text):
                    self._dict_names['module'].append(name)
            except:
                pass
            try:
                for name in get_names_subroutine(text):
                    self._dict_names['subroutine'].append(name)
            except:
                pass
            try:
                for name in get_names_function(text):
                    self._dict_names['function'].append(name)
            except:
                pass

            self.run_single(filename)
            i += 1
    # ...

    # ...
    def run_single(self, filename):
        """
        runs the process for a single file
        """
        source = self.dict_text[filename]

        if self.verbose > 0:
            print (self.dict_names)

        # ... parse, replace,
        for key, values in self.dict_names.items():
            keyword = key

            for block_name in values:
                if self.verbose > 0:
                    print (">>>>> block_name:" + block_name)

                if len(block_name) > 0:
                    constructor = self.dict_constructor[keyword]
                    block = constructor(TAG=block_name, \
                                        source=source, \
                                        filename=filename, \
                                        verbose=self.verbose)
                    block.get_code()
#                    print "------------------------"
#                    print block.source
#                    print "------------------------"
                    if block.is_valid:
                        block.get_signature()
                        block.get_arguments()
                        block.get_decl_call()
                        block.parse_variables()

                        # ... append block in the blocks list
                        self.append(block)
                        # ...

                source = block.source
        self._dict_text[filename] = source
        # ...

        # ... update labels
        for block in self.blocks:
            block.update_label(self)
        # ...

        # ... update variables
        update_variables = self.dict_attribut["update_variables"]
        if update_variables:
            source = self.dict_text[filename]
            for block in self.blocks:
                block.set_source(source)
                block.get_code()
#                print [(v.prefix, v.name) for v in block.variables]
                for var in block.variables:
                    block.replace_variable(var)
                block.update_source()
                source = block.source
            self._dict_text[filename] = source
        # ...

        # ... update Graph
        for block in self.blocks:
            block.update_graph_decl(self)
            block.update_graph_call(self)
        # ...
# ...

