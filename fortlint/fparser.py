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
        self._dict_block       = {}
        self._prefix           = None
        # ... contains the source code for each filename
        self._dict_text        = {}

        if (filename is not None) and (dirname is not None):
            print ("Parser cannot be called with both filename and dirname not empty")
            raise()

        if filename is not None:
            f = open(filename, 'r')
            progtext = f.read()
            self._dict_text[filename] = progtext
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
                            self._dict_text[_filename] = progtext
                            f.close()

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
    def dict_block(self):
        return self._dict_block

    @property
    def dict_text(self):
        return self._dict_text

    def append(self, filename, block):
        self._dict_block[filename].append(block)

    def get_block_by_id(self, ID):
        for filename, blocks in self.dict_block.items():
            for b in blocks:
                if id(b) == ID:
                    return b
        return None

    def get_block_by_filename_name(self, filename, name):
        blocks = self.dict_block[filename]
        for b in blocks:
            if b.name.lower() == name.lower():
                return b
        return None

    def get_blocks_by_name(self, name):
        blocks_out = []
        for filename, blocks in self.dict_block.items():
            for b in blocks:
                if b.name.lower() == name.lower():
                    blocks_out.append(b)
        return blocks_out

    def get_block_by_label(self, label):
        for filename, blocks in self.dict_block.items():
            for b in blocks:
                if b.label.lower() == label.lower():
                    return b
        return None

    # ...
    def run(self):
        for filename, text in self.dict_text.items():
            # ... init dict names
            self._dict_names[filename,'module']     = []
            self._dict_names[filename,'subroutine'] = []
            self._dict_names[filename,'function']   = []
            # ...

        for filename, text in self.dict_text.items():
            try:
                for name in get_names_module(text):
                    self._dict_names[filename, 'module'].append(name)
            except:
                pass
            try:
                for name in get_names_subroutine(text):
                    self._dict_names[filename, 'subroutine'].append(name)
            except:
                pass
            try:
                for name in get_names_function(text):
                    self._dict_names[filename, 'function'].append(name)
            except:
                pass

            self._dict_block[filename] = []
            self.run_single(filename)

        self.update()
    # ...

    # ...
    def run_single(self, filename):
        """
        runs the process for a single file
        """
        source = self.dict_text[filename]

        if self.verbose > 0:
            print ("Run process on :", filename)
            print (self.dict_names)

        # ... parse, replace,
        for keys, values in self.dict_names.items():
            _filename = keys[0]
            keyword  = keys[1]
            if _filename == filename:
                for block_name in values:
                    if self.verbose > 0:
                        print (">>>>> block_name:" + block_name)

                    if len(block_name) > 0:
                        prefix_lib = None
                        constructor = self.dict_constructor[keyword]
                        block = constructor(TAG=block_name, \
                                            source=source, \
                                            filename=filename, \
                                            prefix=self.dict_attribut["prefix"], \
                                            prefix_lib=prefix_lib, \
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
                            self.append(filename, block)
                            # ...

                    source = block.source
        self._dict_text[filename] = source
        # ...

    def update(self):
        # ... update labels
        for filename, blocks in self.dict_block.items():
            for block in blocks:
                block.update_label(self)
        # ...

        # ... update variables
        update_variables = self.dict_attribut["update_variables"]
        for filename, blocks in self.dict_block.items():
            source = self.dict_text[filename]
            blocks1 = [b for b in blocks if b.keyword=="module"]
            blocks2 = [b for b in blocks if not b.keyword=="module"]
            for _blocks in [blocks1, blocks2]:
                for block in _blocks:
                    block.set_source(source)
                    block.get_code()
                    if update_variables:
                        block.update_variables()
                    block.update_source()
                    source = block.source
            self._dict_text[filename] = source
        # ...

        # ... update Graph
        for filename, blocks in self.dict_block.items():
            for block in blocks:
                block.update_graph_decl(self)
                block.update_graph_call(self)
        # ...

    def save_files(self, in_place=False, suffix=".FORTLINT"):
        for filename, text in self.dict_text.items():
            if in_place:
                filename_out    = filename + suffix
            else:
                dirname = os.path.dirname(os.path.realpath(filename))
                dirname_out = dirname + "/_tmp"
                os.system("mkdir -p " + dirname_out)
                filename_out = dirname_out + "/" + filename.split("/")[-1] + suffix

            f = open(filename_out, 'w')
            f.write(text)

# ...

