# Copyright (c) 2014 Danyil Bohdan
# This code is released under the MIT license. See the file LICENSE.

from __future__ import print_function

import unittest
from jsonwatch.jsondiff import *


class JSONDiffTestCase(unittest.TestCase):

    def setUp(self):
        # Based on data from
        # http://api.openweathermap.org/data/2.5/weather\?q\=Kiev,ua.
        self.data1 = {'clouds': {'all': 92}, 'name': 'Kiev', 'coord': {
        'lat': 50.43, 'lon': 30.52}, 'sys': {'country': 'UA',
        'message': 0.0051, 'sunset': 1394985874, 'sunrise': 1394942901
        }, 'weather': [{'main': 'Snow', 'id': 612, 'icon': '13d',
        'description': 'light shower sleet'}, {'main': 'Rain', 'id':
        520, 'icon': '09d', 'description': 'light intensity shower rain'}],
        'rain': {'3h': 2}, 'base': 'cmc stations', 'dt':
        1394979003, 'main': {'pressure': 974.8229, 'humidity': 91,
        'temp_max': 277.45, 'temp': 276.45, 'temp_min': 276.15}, 'id'
        : 703448, 'wind': {'speed': 10.27, 'deg': 245.507}, 'cod':
        200}
        self.data2 = {'clouds': {'all': 92}, 'name': 'Kyiv', 'coord': {
        'lat': 50.43, 'lon': 30.52}, 'sys': {'country': 'UA',
        'message': 0.0051, 'sunset': 1394985874, 'sunrise': 1394942901
        }, 'weather': [{'main': 'Snow', 'id': 612, 'icon': '13d',
        'description': 'light shower sleet'}], 'rain': {'3h': 2},
        'base': 'cmc stations', 'dt':
        1394979003, 'main': {'pressure': 974.8229, 'humidity': 91,
        'temp_max': 277.45, 'temp': 276.45, 'temp_min': 276.15}, 'id'
        : 703448, 'wind': {'speed': 10.27, 'deg': 245.507}, 'cod':
        200}
        self.rec1 = {'surface': {'underground':
                                {'deeper': 'strange things'}}}

    def test_flatten(self):
        self.assertEqual(json_flatten(self.rec1),
                         {'.surface.underground.deeper':
                          'strange things'})

    def test_diff(self):
        self.assertEqual(json_flat_diff(json_flatten(self.data1),
                                        json_flatten(self.data2)),
                         ({'.weather[1].icon': '09d',
                           '.weather[1].main': 'Rain',
                           '.weather[1].description':
                           'light intensity shower rain',
                           '.weather[1].id': 520,
                           '.name': 'Kiev'},
                          {'.weather[1].icon': None,
                           '.weather[1].main': None,
                           '.weather[1].description': None,
                           '.weather[1].id': None,
                           '.name': 'Kyiv'}))
        self.assertEqual(json_flat_diff(json_flatten(self.data1), {})[0],
                         json_flat_diff({}, json_flatten(self.data1))[1])


def suite():
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    suite.addTest(loader.loadTestsFromTestCase(JSONDiffTestCase))
    return suite


if __name__ == '__main__':
    unittest.TextTestRunner(verbosity=2).run(suite())
