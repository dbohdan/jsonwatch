# Copyright 2014 Danyil Bohdan

from __future__ import print_function

def json_flatten(a, prefix=''):
    def append_flat(list_, elem):
        if isinstance(elem, list):
            list_.extend(elem)
        else:
            list_.append(elem)
        
    res = []
    if isinstance(a, list):
        for n, el in enumerate(a):
            append_flat(res, json_flatten(el, prefix + str(n)))
    elif isinstance(a, dict):
        for key in a.keys():
            append_flat(res, json_flatten(a[key], prefix + ".{0}".format(key)))
    else:
        res = (a, prefix)
            
    return res

def json_diff(a, b):
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
        a_keys = set(a.keys())
        b_keys = set(b.keys())
        common_keys = a_keys.intersection(b_keys)
        a_only_keys = a_keys - common_keys
        b_only_keys = b_keys - common_keys
        
        for key in common_keys:
            d = json_diff(a[key], b[key])
            if d is not None:
                res[key] = d
                
        for key in a_only_keys:
            res[key] = (a[key], None)
            
        for key in b_only_keys:
            res[key] = (None, b[key])
            
        if res == {}:
            res = None
    else:
        if a != b:
            res = (a, b)
            
    return res

def tests():
    # Based on data from http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua.
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
    data2 = {u'clouds': {u'all': 92}, u'name': u'Kyiv', u'coord': {
    u'lat': 50.43, u'lon': 30.52}, u'sys': {u'country': u'UA', 
    u'message': 0.0051, u'sunset': 1394985874, u'sunrise': 1394942901
    }, u'weather': [{u'main': u'Snow', u'id': 612, u'icon': u'13d', 
    u'description': u'light shower sleet'}, {u'main': u'Lain', u'id':
    520, u'icon': u'09d', u'description': u'light intensity shower \
    rain'}], u'rain': {u'3h': 2}, u'base': u'cmc stations', u'dt': 
    1394979003, u'main': {u'pressure': 974.8229, u'humidity': 91, 
    u'temp_max': 277.45, u'temp': 276.45, u'temp_min': 276.15}, u'id'
    : 703448, u'wind': {u'speed': 10.27, u'deg': 245.507}, u'cod': 
    200}
    data3 = {u'clouds': {u'all': 92}}    
    print(json_diff(data1, data2) == {u'weather': [{u'main': (u'Rain'
    , u'Lain')}], u'name': (u'Kiev', u'Kyiv')})
    #print(json_diff(data1, data3), "\n", json_diff(data3, data1))
    #print("---\n\n", json_flatten(data1))

if __name__ == '__main__':
    tests()
