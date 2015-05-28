__author__ = 'macpro'
from math import radians, cos, sin, asin, sqrt, pi, atan2


def haversine(lon1, lat1, lon2, lat2):
    """ Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat / 2) ** 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ** 2
    c = 2 * asin(sqrt(a))
    r = 6371
    # Radius of earth in kilometers. Use 3956 for miles
    return c * r


def haversineKaggle(lon1, lat1, lon2, lat2):
    r = 6371
    #lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    dlon = abs(lon2 - lon1)
    dlat = abs(lat2 - lat1)
    a = sin((pi * dlat) / 360) ** 2 + cos(lat1) * cos(lat2) * sin((pi * dlon) / 360) ** 2
    c = 2 * r * atan2(sqrt(a), sqrt(1 - a))
    return c


def haversineKaggleFromR(lon1, lat1, lon2, lat2):
    REarth = 6371
    lat = abs(lat1 - lat2) * pi / 180
    lon = abs(lon1 - lon2) * pi / 180
    lat1 = lat1 * pi / 180
    lat2 = lat2 * pi / 180
    a = sin(lat / 2) * sin(lat / 2) + cos(lat1) * cos(lat2) * sin(lon / 2) * sin(lon / 2)
    d = 2 * atan2(sqrt(a), sqrt(1 - a))
    d = REarth * d
    return (d)





print(haversine(-8.630838, 41.154489, -8.66574, 41.170671))
print(haversineKaggle(-8.630838, 41.154489, -8.66574, 41.170671))
print(haversineKaggleFromR(-8.630838, 41.154489, -8.66574, 41.170671))