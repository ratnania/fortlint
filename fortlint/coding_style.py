# -*- coding: utf8 -*-

import re
import os
from constants import *
from declaration import Variable
from blocks import Block
from blocks import SubroutineBlock
from blocks import FunctionBlock
from blocks import ModuleBlock

# ...
list_keywords_decs = ['integer', 'real', 'type', 'class']

dict_keywords_re = {}
for word in list_keywords_decs:
    pattern ="[ ]*" + word + "[^:]*::\s+(.+)"

    word_re = re.compile(pattern)
    dict_keywords_re[word] = word_re
# ...

# ...
class VariableJorek(Variable):
    def __init__(self, *args, **kwargs):
        Variable.__init__(self, *args, **kwargs)

    def construct_prefix(self):
        # ... check if the variable has already a prefix
#        print ">>> name :", self.name
        try:
            prefix = self.name.split('_',1)[0]
            if len(prefix) in [2,3]:
                self._name = self.name.split('_')[1]
        except:
            pass
        # ...
        if self.ctype is None:
            print ("ctype is None.")
            raise()
        prefix = self.ctype
        if self.is_array:
            prefix += "p"

        dtype = ""
        if self.dtype.lower() == "integer":
            dtype = PREFIX_DECLARATION_TYPE_INTEGER
        if self.dtype.lower() == "real":
            dtype = PREFIX_DECLARATION_TYPE_REAL
        if self.dtype.lower() == "character":
            dtype = PREFIX_DECLARATION_TYPE_CHARACTER
        if self.dtype.lower() == "logical":
            dtype = PREFIX_DECLARATION_TYPE_LOGICAL
        if self.dtype.lower() == "type":
            dtype = PREFIX_DECLARATION_TYPE_TYPE
        if self.dtype.lower() == "class":
            dtype = PREFIX_DECLARATION_TYPE_CLASS

        prefix += dtype

        self._prefix = prefix + "_"

#        print self._prefix

    def __str__(self):
        return 'Name={0}, Type={1}'.format(self.name, self.dtype)
# ...

# ...
class BlockJorek(Block):
    """

    """
    def __init__(self, *args, **kwargs):
        Block.__init__(self, *args, **kwargs)

    def parse_variables(self):
        Block.parse_variables(self, constructor_variable=VariableJorek)
#        print "---"
#        print self._arguments
#        print [v.name for v in self.variables]

        # ... arguments
        for var in self._variables:
        # TODO must treate all cases
            if var.name in self._arguments:
                var.set_ctype(PREFIX_CONTEXT_TYPE_ARGUMENT)
            else:
                var.set_ctype(PREFIX_CONTEXT_TYPE_LOCAL)
        # ...

    def replace_variable(self, var, inline=True):
        """
        """
        name = var.name
        _text = self.text

        pattern = r"\b"+ name + r"\b"
        print ">>> pattern :", pattern
        _text = re.sub(pattern, var.prefix + name.lower(), _text)

        if inline:
            self._text = _text
        else:
            return _text

    def update_source(self):
        list_code = self._re_block.split(self.source)
        list_code_new = []
        list_code_new.append(list_code[0])
        code_inner   = self.keyword.upper() + " " + self.prefix.upper() + self.name.upper() \
                     + self.text \
                     + " END " + self.keyword.upper() + " " + self.name.upper()
        list_code_new.append(code_inner)
        for code in list_code[2:]:
            list_code_new.append(code)

        self._source = ''.join(list_code_new)
#        print self.keyword
#        if self.keyword == "subroutine":
#            print "XXXXXXXXXXXXXXXXXXXXXXXX"
        if self.verbose > 1:
            print self._source
#        print self._source
# ...

# ...
class SubroutineBlockJorek(BlockJorek, SubroutineBlock):
    def __init__(self, TAG="", \
                 source=None, \
                 filename=None, \
                 prefix=None, \
                 prefix_lib=None, \
                 verbose=0):
        BlockJorek.__init__(self, "subroutine", \
                            TAG=TAG, \
                            source=source, \
                            verbose=verbose, \
                            prefix=prefix, \
                            prefix_lib=prefix_lib, \
                            filename=filename)
# ...

# ...
class FunctionBlockJorek(BlockJorek, FunctionBlock):
    def __init__(self, TAG="", \
                 source=None, \
                 filename=None, \
                 prefix=None, \
                 prefix_lib=None, \
                 verbose=0):
        BlockJorek.__init__(self, "function", \
                            TAG=TAG, \
                            source=source, \
                            verbose=verbose, \
                            prefix=prefix, \
                            prefix_lib=prefix_lib, \
                            filename=filename)
# ...

# ...
class ModuleBlockJorek(BlockJorek, ModuleBlock):
    def __init__(self, TAG="", \
                 source=None, \
                 filename=None, \
                 prefix=None, \
                 prefix_lib=None, \
                 verbose=0):
        BlockJorek.__init__(self, "module", \
                            TAG=TAG, \
                            source=source, \
                            verbose=verbose, \
                            prefix=prefix, \
                            prefix_lib=prefix_lib, \
                            filename=filename)
# ...

