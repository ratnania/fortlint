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
        return self._prefix

    def set_name(self, name):
        self._name = name

    def set_dtype(self, dtype):
        self._dtype = dtype

    def set_ctype(self, ctype):
        self._ctype = ctype

    def construct_prefix(self):
        self._prefix = ""


    def __str__(self):
        return 'Name={0}, Type={1}'.format(self.name, self.dtype)
# ...

