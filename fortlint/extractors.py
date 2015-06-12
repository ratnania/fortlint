# -*- coding: utf8 -*-

import re
import os

# ...
list_keywords_decs = ['integer', 'real', 'type', 'class']

dict_keywords_re = {}
for word in list_keywords_decs:
    pattern ="[ ]*" + word + "[^:]*::\s+(.+)"

    word_re = re.compile(pattern, re.I)
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

    word_re = re.compile(pattern, re.I)
    return word_re
# ...

# ...
def extract_arguments():
    pattern ="([\w]*),*"
    word_re = re.compile(pattern, re.I)
    return word_re
# ...

# ...
def get_names_subroutine(text_in):
    # ... find subroutine names
    keyword = "subroutine"
    pattern = r"\b" + keyword + r"\b.*\("
    word_re = re.compile(pattern, re.I)
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
    word_re = re.compile(pattern, re.I)
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
def get_names_module(text_in):
    # ... find module names
    keyword = "module"
    pattern = r"\b" + keyword + r"\b.*"
    word_re = re.compile(pattern, re.I)
    text = word_re.findall(text_in)
#    print ("+++++ text :", text)
    list_names = []
    for t in text:
        list_s = [s for s in t.split(keyword) if len(s) > 0]
#        print ("++ list_s :", list_s)
        for s in list_s:
            list_d = [d.rstrip().lstrip() for d in s.split("(") if len(d) > 0]
            list_names.append(list_d[0])
    set_names = set(list_names)
    list_names = list(set_names)
#    print ("+++++ modules-names :", list_names)
    return list_names
# ...

# ...
def get_calls_subroutine(source):
    _re = extract_subroutine_call()
    return _re.findall(source)
# ...

# ...
def get_calls_function(source):
    _re = extract_function_call()
    return _re.findall(source)
# ...

# ...
def extract_subroutine_call():
    pattern = r"\bcall\s+(\w+)\("
    word_re = re.compile(pattern, re.I)
    return word_re
# ...

# ...
def extract_function_call():
    pattern = r"\b(\w+)\("
    word_re = re.compile(pattern, re.I)
    return word_re
# ...

# ...
def extract_contains():
    pattern = r"\bcontains\b"

    word_re = re.compile(pattern, re.I)
    return word_re
# ...


# ...
def get_declarations_calls(source):
#    print ("===========================")
#    print (">>> Enter get_declarations_calls")
#    print (">>> source ")
#    print (source)

    text = source

    _re = extract_contains()
    condition = (len(_re.findall(text)) > 0)

    dict_decl = {}
    dict_calls = {}
    if condition:
        list_code = _re.split(text)
        for code in list_code:
            print ">>> ", code


        # ... get calls - subroutines
        calls_sub = get_calls_subroutine(list_code[0])
        # ...

        # ... get calls - functions
        calls_fun = get_calls_function(list_code[0])
        # ...

        # ... put back the other contains
        list_code_new = []
        code = list_code[1]
        list_code_new.append(r"\tcontains \n")
        list_code_new.append(code)
        text = ''.join(list_code_new[:])
        # ...

        # ... get declaration - subroutines
        names_sub = get_names_subroutine(text)
        # ...

        # ... get declaration - functions
        names_fun = get_names_function(text)
        # ...

        # ...
        dict_decl["subroutine"] = names_sub
        dict_decl["function"]   = names_fun
        # ...

        # ...
        dict_calls["subroutine"] = calls_sub
        dict_calls["function"]   = calls_fun
        # ...

    return dict_decl, dict_calls
# ...



