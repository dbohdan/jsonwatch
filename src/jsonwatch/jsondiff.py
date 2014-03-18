# Copyright 2014 Danyil Bohdan

def json_flatten(a, prefix=''):
    """Flatten a JSON structure into a dict."""
    def add_flat(dict_, key, elem):
        if isinstance(elem, dict):
            dict_.update(elem)
        else:
            dict_[key] = elem

    res = {}
    if isinstance(a, list):
        for n, elem in enumerate(a):
            add_flat(res, prefix, json_flatten(elem, \
                                               prefix + "[{0}]".format(n)))
    elif isinstance(a, dict):
        for key in a.keys():
            new_prefix = prefix
            # Account for possible spaces in keys.
            if ' ' in key:
                new_prefix += "['{0}']".format(key)
            else:
                new_prefix += ".{0}".format(key)
            add_flat(res, prefix, json_flatten(a[key], new_prefix))
    elif a is not None and prefix != '':
        res[prefix] = a
    else:
        res = a

    return res


def c_keys(a, b):
    a_keys = set(a.keys())
    b_keys = set(b.keys())
    common_keys = a_keys.intersection(b_keys)
    return common_keys, a_keys, b_keys, \
            a_keys - common_keys, b_keys - common_keys


def remove_none_values(dict_):
    res = {}
    res.update((key, value) for key, value in dict_.iteritems() \
                if value is not None)
    return res


def json_flat_diff(a, b):
    res_a = {}
    res_b = {}
    for key in set(a.keys()).union(set(b.keys())):
        a_value = a.get(key)
        b_value = b.get(key)
        if a_value != b_value:
            res_a[key] = a_value
            res_b[key] = b_value
    # Mind the parenthesis below lest you return ({}, None).
    return (res_a, res_b) if res_a != {} else None


def json_diff_str(diff):
    res = []
    flat_diff_from, flat_diff_to = diff
    flat_diff_from = remove_none_values(flat_diff_from)
    flat_diff_to = remove_none_values(flat_diff_to)
    common_keys, _, _, from_keys, to_keys = c_keys(flat_diff_from, flat_diff_to)
    for key in from_keys:
        res.append("- {0}: {1}".format(key, flat_diff_from[key]))
    for key in common_keys:
        res.append("{0}: {1} -> {2}".format(key, flat_diff_from[key], \
                                            flat_diff_to[key]))
    for key in to_keys:
        res.append("+ {0}: {1}".format(key, flat_diff_to[key]))
    return res
