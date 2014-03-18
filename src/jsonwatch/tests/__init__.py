# Copyright (c) 2014 Danyil Bohdan

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.


from __future__ import print_function

import unittest
from jsonwatch.jsondiff import *


class JSONDiffTestCase(unittest.TestCase):

    def setUp(self):
        # Based on data from
        # http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua.
        self.data1 = {u'clouds': {u'all': 92}, u'name': u'Kiev', u'coord': {
        u'lat': 50.43, u'lon': 30.52}, u'sys': {u'country': u'UA',
        u'message': 0.0051, u'sunset': 1394985874, u'sunrise': 1394942901
        }, u'weather': [{u'main': u'Snow', u'id': 612, u'icon': u'13d',
        u'description': u'light shower sleet'}, {u'main': u'Rain', u'id':
        520, u'icon': u'09d', u'description': u'light intensity shower rain'}],
        u'rain': {u'3h': 2}, u'base': u'cmc stations', u'dt':
        1394979003, u'main': {u'pressure': 974.8229, u'humidity': 91,
        u'temp_max': 277.45, u'temp': 276.45, u'temp_min': 276.15}, u'id'
        : 703448, u'wind': {u'speed': 10.27, u'deg': 245.507}, u'cod':
        200}
        self.data2 = {u'clouds': {u'all': 92}, u'name': u'Kyiv', u'coord': {
        u'lat': 50.43, u'lon': 30.52}, u'sys': {u'country': u'UA',
        u'message': 0.0051, u'sunset': 1394985874, u'sunrise': 1394942901
        }, u'weather': [{u'main': u'Snow', u'id': 612, u'icon': u'13d',
        u'description': u'light shower sleet'}], u'rain': {u'3h': 2},
        u'base': u'cmc stations', u'dt':
        1394979003, u'main': {u'pressure': 974.8229, u'humidity': 91,
        u'temp_max': 277.45, u'temp': 276.45, u'temp_min': 276.15}, u'id'
        : 703448, u'wind': {u'speed': 10.27, u'deg': 245.507}, u'cod':
        200}
        self.rec1 = {u'surface': {u'underground':
                                 {u'deeper': 'strange things'}}}

    def test_flatten(self):
        self.assertEqual(json_flatten(self.rec1),
                         {'.surface.underground.deeper':
                          'strange things'})

    def test_diff(self):
        self.assertEqual(json_flat_diff(json_flatten(self.data1),
                                        json_flatten(self.data2)),
                         ({'.weather[1].icon': u'09d',
                           '.weather[1].main': u'Rain',
                           '.weather[1].description':
                           u'light intensity shower rain',
                           '.weather[1].id': 520,
                           '.name': u'Kiev'},
                          {'.weather[1].icon': None,
                           '.weather[1].main': None,
                           '.weather[1].description': None,
                           '.weather[1].id': None,
                           '.name': u'Kyiv'}))
        self.assertEqual(json_flat_diff(json_flatten(self.data1), {})[0],
                         json_flat_diff({}, json_flatten(self.data1))[1])


def suite():
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    suite.addTest(loader.loadTestsFromTestCase(JSONDiffTestCase))
    return suite


if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite())
