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
import re
import os

# ...
class Parser(object):
    def __init__(self, filename=None, dirname=None, \
                dict_constructor=None, \
                verbose=0):
        self._filename = filename
        self._dirname  = dirname
        self._text     = None
        self._dict_names = {}
        self._dict_constructor = dict_constructor
        self._verbose   = verbose

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

    # ...
    def update_variables(self):
        source = self.text

        if self.verbose > 0:
            print (self.dict_names)

        # ... subroutine case
        for block_name in self.dict_names['subroutine']:
            if self.verbose > 0:
                print (">>>>> block_name:" + block_name)
#            print "-------------------------------------------------"
#            print source
#            print "-------------------------------------------------"

            block =  self.dict_constructor["subroutine"](TAG=block_name, source=source)
            block.get_signature()
            block.get_arguments()
            block.parse_variables()
            for var in block.variables:
                block.replace_variable(var)
            block.update_source()

            source = block.source
#            print "+++++++++++++++++++++++++++++++++++++++++++++++++"
#            print source
#            print "+++++++++++++++++++++++++++++++++++++++++++++++++"
        # ...

        # ... subroutine case
        for block_name in self.dict_names['function']:
            if self.verbose > 0:
                print (">>>>> block_name:"+ block_name)
            block = self.dict_constructor["function"](TAG=block_name, source=source)
            block.get_signature()
            block.get_arguments()
            block.parse_variables()
            for var in block.variables:
                block.replace_variable(var)
            block.update_source()

            source = block.source
#            print "+++++++++++++++++++++++++++++++++++++++++++++++++"
#            print block.source
#            print "+++++++++++++++++++++++++++++++++++++++++++++++++"
        # ...

        self._text = source
# ...

