# -*- coding: utf8 -*-

import re
import os

# ...
list_keywords_decs = ["integer", "real", "logical", "type", "class"]

dict_keywords_re = {}
for word in list_keywords_decs:
#    pattern = r"[ ]*\b" + word + r"\b\s*::\s*"
    pattern = r"[ ]*" + word + r"[^:]*::\s*(.+)"

    word_re = re.compile(pattern, re.DOTALL | re.I)
    dict_keywords_re[word] = word_re
# ...

# ...
def extract_blocks(word, TAG):
    pattern_start = word + "\s+" + TAG
    pattern_end   = "end\s+" + word + r"\s+\b" + TAG + r"\b"

    pattern = pattern_start + "(.*?)" + pattern_end
    word_re = re.compile(pattern, re.DOTALL | re.I)
    return word_re
# ...

# ...
def extract_signature():
    pattern = r"[^:]*\(.+\)"
#    pattern ="\(.+\)"
#    pattern =r"\s*\(\s*.*\)"

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
    text = word_re.findall(text_in.lower(), re.I)
    list_names = []
    for t in text:
        list_s = [s for s in t.split(keyword) if len(s) > 0]
        for s in list_s:
            list_d = [d.rstrip().lstrip() for d in s.split("(") if len(d) > 0]
            list_names.append(list_d[0])
#    print ("+++++ subroutine-names :", list_names)
    list_names = [name.lower() for name in list_names if len(name) > 0]
    return list_names
# ...

# ...
def get_names_function(text_in):
    # ... find function names
    keyword = "function"
    pattern = r"\b" + keyword + r"\b.*\("
    word_re = re.compile(pattern, re.I)
    text = word_re.findall(text_in.lower(), re.I)
    list_names = []
    for t in text:
        list_s = [s for s in t.split(keyword) if len(s) > 0]
        for s in list_s:
            list_d = [d.rstrip().lstrip() for d in s.split("(") if len(d) > 0]
            list_names.append(list_d[0])
#    print ("+++++ function-names :", list_names)
    list_names = [name.lower() for name in list_names if len(name) > 0]
    return list_names
# ...

# ...
def get_names_module(text_in):
    # ... find module names
    keyword = "module"
    pattern = r"\b" + keyword + r"\b.*"
    word_re = re.compile(pattern, re.I)
    text = word_re.findall(text_in.lower(), re.I)
    list_names = []
    for t in text:
        list_s = [s for s in t.split(keyword) if len(s) > 0]
#        print ("++ list_s :", list_s)
        for s in list_s:
            list_d = [d.rstrip().lstrip() for d in s.split("(") if len(d) > 0]
            list_names.append(list_d[0])
    set_names = set(list_names)
    list_names = list(set_names)
    list_names = [name.lower() for name in list_names if len(name) > 0]
#    print ("+++++ modules-names :", list_names)
    return list_names
# ...

# ...
def get_calls_subroutine(source):
    _re = extract_subroutine_call()
    return _re.findall(source, re.I)
# ...

# ...
def get_calls_function(source):
    _re = extract_function_call()
    return _re.findall(source, re.I)
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
    text = source

    _re = extract_contains()
    condition = (len(_re.findall(text, re.I)) > 0)

    dict_decl = {}
    dict_calls = {}
    if condition:
        list_code = _re.split(text)

        # ... get calls - subroutines
        calls_sub = get_calls_subroutine(list_code[0])
        # ...

        # ... get calls - functions
        _calls_fun = get_calls_function(list_code[0])
        calls_fun = [s for s in _calls_fun if s not in calls_sub]
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
    else:
        # ... get calls - subroutines
        calls_sub = get_calls_subroutine(text)
        # ...

        # ... get calls - functions
        _calls_fun = get_calls_function(text)
        calls_fun = [s for s in _calls_fun if s not in calls_sub]
        # ...

        # ...
        dict_decl["subroutine"] = []
        dict_decl["function"]   = []
        # ...

        # ...
        dict_calls["subroutine"] = calls_sub
        dict_calls["function"]   = calls_fun
        # ...

    return dict_decl, dict_calls
# ...

# ...
def get_signature_from_text(source):
    _re = extract_signature()
    m = _re.match(source)
    t = m.group(0)
    return t
# ...

# ...
def get_arguments_from_text(source):
    text = source
    try:
        data = extract_arguments().findall(text.lstrip(), re.I)
        arguments = [b.rstrip().lstrip() for b in data if len(b) > 0]
    except:
        arguments = []

    return arguments
# ...

# ...
def get_declarations_variables(source, constructor):
    _source = source.lower()
    list_var = []
    for word in list_keywords_decs:
        pattern = r"[ ]*" + word + r"[^:]*::\s*(.+)"
        pattern = r"[ ]*" + word + r"(.*dimension)?.*::\s*(.+)"

        _re = re.compile(pattern,re.I)
        _vars_name = _re.findall(_source, re.I)
        print _vars_name
        try:
            _vars_arg,_vars_name = zip(*_vars_name)
        except:
            _vars_name = [] ; _vars_arg = None
        print _vars_name, " ---- ",_vars_arg
#        _vars_name = _re.match(_source).group(-1)
        if len(_vars_name) > 0:
            for _vars, _args in zip(_vars_name, _vars_arg):
                for var_name in _vars.split(','):
                    args = _args.split(',')
                    args = [s.strip() for s in args if len(s)>0]
                    print var_name, " --- ARGS :", args
                    var = constructor(name=var_name.strip(), \
                                        dtype=word, \
                                        attributs=args)
                    list_var.append(var)
    return list_var
# ...



