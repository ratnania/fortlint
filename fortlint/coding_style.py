# -*- coding: utf8 -*-

import re
import os
from constants import PREFIX_DECLARATION_TYPE_INTEGER
from constants import PREFIX_DECLARATION_TYPE_REAL
from constants import PREFIX_DECLARATION_TYPE_CHARACTER
from constants import PREFIX_DECLARATION_TYPE_LOGICAL
from constants import PREFIX_CONTEXT_TYPE_LOCAL
from constants import PREFIX_CONTEXT_TYPE_ARGUMENT
from constants import PREFIX_CONTEXT_TYPE_MODULE
from constants import PREFIX_CONTEXT_TYPE_OBJECT
from constants import PREFIX_CONTEXT_TYPE_GLOBAL
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
        prefix += dtype

        self._prefix = prefix + "_"


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

        # ... arguments
        for var in self._variables:
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
        _text = re.sub(pattern, var.prefix + name.lower(), _text)


        if inline:
            self._text = _text
        else:
            return _text

    def update_source(self):
        list_code = self._re_block.split(self.source)
        list_code_new = []
        list_code_new.append(list_code[0])
        code_inner   = self.keyword.upper() + " " + self.name.upper() \
                     + self.text \
                     + " END " + self.keyword.upper() + " " + self.name.upper()
        list_code_new.append(code_inner)
        for code in list_code[2:]:
            list_code_new.append(code)

        self._source = ''.join(list_code_new)
# ...

# ...
class SubroutineBlockJorek(BlockJorek, SubroutineBlock):
    def __init__(self, TAG="", source=None):
        BlockJorek.__init__(self, "subroutine", TAG=TAG, source=source)
# ...

# ...
class FunctionBlockJorek(BlockJorek, FunctionBlock):
    def __init__(self, TAG="", source=None):
        BlockJorek.__init__(self, "function", TAG=TAG, source=source)
# ...

# ...
class ModuleBlockJorek(BlockJorek, ModuleBlock):
    def __init__(self, TAG="", source=None):
        BlockJorek.__init__(self, "module", TAG=TAG, source=source)
# ...

