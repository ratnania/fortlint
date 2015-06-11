# -*- coding: utf8 -*-

import re
import os

# ...
class Variable(object):
    def __init__(self, name=None, dtype=None, ctype=None):
        self._name   = name
        self._dtype  = dtype
        self._ctype  = ctype
        self._prefix = None
        self._is_array = False

    @property
    def name(self):
        return self._name

    @property
    def dtype(self):
        return self._dtype

    @property
    def ctype(self):
        return self._ctype

    @property
    def is_array(self):
        return self._is_array

    @property
    def prefix(self):
        if self._prefix is None:
            self.construct_prefix()
#        print "---"
#        print self._prefix
#        print "---"
        return self._prefix

    def set_name(self, name):
        self._name = name

    def set_dtype(self, dtype):
        self._dtype = dtype

    def set_ctype(self, ctype):
        self._ctype = ctype

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

