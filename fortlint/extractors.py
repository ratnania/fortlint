# -*- coding: utf8 -*-

import re
import os

# ...
list_keywords_decs = ['integer', 'real', 'type', 'class']

dict_keywords_re = {}
for word in list_keywords_decs:
    pattern ="[ ]*" + word + "[^:]*::\s+(.+)"

    word_re = re.compile(pattern)
    dict_keywords_re[word] = word_re
# ...

# ...
def extract_blocks(word, TAG):
    pattern_start = word + "\s+" + TAG
    pattern_end   = "end\s+" + word + r"\s+\b" + TAG + r"\b"

    pattern = pattern_start + "(.*?)" + pattern_end
    word_re = re.compile(pattern, re.DOTALL)
    return word_re
# ...

# ...
def extract_signature():
    pattern ="[^:]*\(.+\)"

    word_re = re.compile(pattern)
    return word_re
# ...

# ...
def extract_arguments():
    pattern ="([\w]*),*"
    word_re = re.compile(pattern)
    return word_re
# ...

# ...
def get_names_subroutine(text_in):
    # ... find subroutine names
    keyword = "subroutine"
    pattern = r"\b" + keyword + r"\b.*\("
    word_re = re.compile(pattern)
    text = word_re.findall(text_in)
    list_names = []
    for t in text:
        list_s = [s for s in t.split(keyword) if len(s) > 0]
        for s in list_s:
            list_d = [d.rstrip().lstrip() for d in s.split("(") if len(d) > 0]
            list_names.append(list_d[0])
    return list_names
# ...

# ...
def get_names_function(text_in):
    # ... find function names
    keyword = "function"
    pattern = r"\b" + keyword + r"\b.*\("
    word_re = re.compile(pattern)
    text = word_re.findall(text_in)
    list_names = []
    for t in text:
        list_s = [s for s in t.split(keyword) if len(s) > 0]
        for s in list_s:
            list_d = [d.rstrip().lstrip() for d in s.split("(") if len(d) > 0]
            list_names.append(list_d[0])
    return list_names
# ...
