# Copyright 2014 Danyil Bohdan

from __future__ import print_function

def json_flat_diff_invert(diff):
    res_a, res_b = {}, {}
    for key in diff:
        res_a[key] = diff[key][0]
        res_b[key] = diff[key][1]
    return res_a, res_b

def json_flatten(a, prefix=''):
    """Flattens a JSON structure into a dict."""
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
    elif isinstance(a, tuple) and len(a) == 2: # For JSON within diffs.
        add_flat(res, prefix, (json_flatten(a[0], ''), \
                               json_flatten(a[1], '')))
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

def json_diff_str(d, flat=False):
    res = []
    if not flat:
        d = json_flatten(d)
    flat_diff_from, flat_diff_to = json_flat_diff_invert(d)
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

def json_diff(a, b):
    """Returns difference between two structures."""
    res = None
    if isinstance(a, list) and isinstance(b, list):
        res = []
        min_len = min(len(a), len(b))

        for i in xrange(min_len):
            d = json_diff(a[i], b[i])
            if d is not None:
                res.append(d)

        for i in xrange(min_len, len(a)):
            d = json_diff(a[i], None)
            if d is not None:
                res.append(d)

        for i in xrange(min_len, len(b)):
            d = json_diff(None, b[i])
            if d is not None:
                res.append(d)

    elif isinstance(a, dict) and isinstance(b, dict):
        res = {}
        common_keys, _, _, a_only_keys, b_only_keys = c_keys(a, b)

        for key in common_keys:
            d = json_diff(a[key], b[key])
            if d is not None:
                res[key] = d

        for key in a_only_keys:
            res[key] = (a[key], None)

        for key in b_only_keys:
            res[key] = (None, b[key])

    else:
        if a != b:
            res = (a, b)

    if res == {} or res == []:
        res = None

    return res

def json_flat_diff(a, b):
    res = {}
    for key in set(a.keys()).union(set(b.keys())):
        a_value = a.get(key)
        b_value = b.get(key)
        if a_value != b_value:
            res[key] = (a_value, b_value)
    return res

def tests():
    # Based on data from
    # http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua.
    data1 = {u'clouds': {u'all': 92}, u'name': u'Kiev', u'coord': {
    u'lat': 50.43, u'lon': 30.52}, u'sys': {u'country': u'UA',
    u'message': 0.0051, u'sunset': 1394985874, u'sunrise': 1394942901
    }, u'weather': [{u'main': u'Snow', u'id': 612, u'icon': u'13d',
    u'description': u'light shower sleet'}, {u'main': u'Rain', u'id':
    520, u'icon': u'09d', u'description': u'light intensity shower \
    rain'}], u'rain': {u'3h': 2}, u'base': u'cmc stations', u'dt':
    1394979003, u'main': {u'pressure': 974.8229, u'humidity': 91,
    u'temp_max': 277.45, u'temp': 276.45, u'temp_min': 276.15}, u'id'
    : 703448, u'wind': {u'speed': 10.27, u'deg': 245.507}, u'cod':
    200}
    #data2 = {u'clouds': {u'all': 92}, u'name': u'Kyiv', u'coord': {
    #u'lat': 50.43, u'lon': 30.52}, u'sys': {u'country': u'UA',
    #u'message': 0.0051, u'sunset': 1394985874, u'sunrise': 1394942901
    #}, u'weather': [{u'main': u'Snow', u'id': 612, u'icon': u'13d',
    #u'description': u'light shower sleet'}, {u'main': u'Lain', u'id':
    #520, u'icon': u'09d', u'description': u'light intensity shower \
    #rain'}], u'rain': {u'3h': 2}, u'base': u'cmc stations', u'dt':
    #1394979003, u'main': {u'pressure': 974.8229, u'humidity': 91,
    #u'temp_max': 277.45, u'temp': 276.45, u'temp_min': 276.15}, u'id'
    #: 703448, u'wind': {u'speed': 10.27, u'deg': 245.507}, u'cod':
    #200}
    #rec1 = {u'surface': {u'underground': {u'deeper': 'strange things'}}}
    #print(json_diff(data1, data2) == {u'weather': [{u'main': (u'Rain'
    #, u'Lain')}], u'name': (u'Kiev', u'Kyiv')})
    #print(json_flat_diff_invert(json_flatten(json_diff(data1, {})))[0] == \
    #      json_flat_diff_invert(json_flatten(json_diff({}, data1)))[1])
    print(json_flat_diff(json_flatten(data1), json_flatten({})))
    print(json_flatten(json_diff(data1, {})))

    #for key in json_flatten(data1):
    #    print(key, ":", json_flatten(data1)[key])

if __name__ == '__main__':
    tests()
