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
class Block(object):
    """

    """
    def __init__(self, keyword, TAG="", source=None, verbose=0):
        self._keyword   = keyword
        self._TAG       = TAG
        self._text      = None
        self._source    = source
        self._variables = None
        self._name      = TAG
        self._verbose   = verbose

        self._re_block     = extract_blocks(self._keyword, TAG)
        self._re_signature = extract_signature()
        self._re_arguments = extract_arguments()

    @property
    def keyword(self):
        return self._keyword

    @property
    def name(self):
        return self._name

    @property
    def text(self):
        return self._text

    @property
    def source(self):
        return self._source

    @property
    def variables(self):
        if self._variables is None:
            self.parse_variables()

        return self._variables

    @property
    def variables_arguments(self):
        return [var for var in self.variables if var.name in self._arguments]

    @property
    def variables_local(self):
        return [var for var in self.variables if var.name not in self._arguments]

    @property
    def verbose(self):
        return self._verbose

    def set_source(self, source):
        self._source = source

    def get_code(self):
        if self.text is None:
            if self.source is not None:
#                print "==========================="
#                n = len(self._re_block.split(self.source))
#                for i in range(0,n):
#                    print ">>>>>", self._re_block.split(self.source)[i]
#                print n
#                print "==========================="
                try:
                    self._text = self._re_block.findall(self.source)[0]
                except:
                    print ("Cannot parse the source code")
                    print (self.source)
                    raise()
#                print ">>> text:", self.text
            else:
                print ("you must provide the source code before parsing")
                raise()

        return self.text

#    def findall(self):
#        return self._re_block.findall(self.text)

    def get_signature(self):
        if self.text is None:
            self.get_code()
        return self._re_signature.findall(self.text)[0]

    def get_arguments(self):
        if self.text is None:
            self.get_code()
        _text = self.get_signature()
        if self.verbose > 0:
            print (">>> signature:", _text)
        data = self._re_arguments.findall(_text)
        self._arguments = [b.rstrip() for b in data if len(b) > 0]
        return self._arguments

    def parse_variables(self, constructor_variable=None):
        if constructor_variable is None:
            print ("parse_variables: a constructor must be provided. Received None")
            raise()
        self._variables = []
        for keyword in list_keywords_decs:
            _re = dict_keywords_re[keyword]
            _vars_name = _re.findall(self.text)
            for var_name in _vars_name:
                var = constructor_variable(name=var_name.rstrip(), dtype=keyword)
                self._variables.append(var)

    def replace_variable(self, var, inline=True):
        """
        """
        print ("replace_variable: not yet implemented for the generic class")
        raise()

    def update_source(self):
        print ("update_source: not yet implemented for the generic class")
        raise()
# ...

# ...
class SubroutineBlock(Block):
    def __init__(self, TAG="", source=None):
        Block.__init__(self, "subroutine", TAG=TAG, source=source)
# ...

# ...
class FunctionBlock(Block):
    def __init__(self, TAG="", source=None):
        Block.__init__(self, "function", TAG=TAG, source=source)
# ...

