# coding: utf-8
import math, copy

import ROOT
#ROOT.gSystem.Load('StarSpinAnalyses')

#import mystyle; mystyle.use(1)
ROOT.gStyle.SetOptStat(0)


class datapoint:
    """docstring for datapoint"""
    def __init__(self, x, xlow, binwidth, y, stat=0.0, sys=0.0):
        self.x          = x
        self.xlow       = xlow
        self.binwidth   = binwidth
        self.y          = y
        self.stat       = stat
        self.sys        = sys
    
    def add(self, other):
        """add values from other into self"""
        assert(self.x == other.x)
        assert(self.xlow == other.xlow)
        assert(self.binwidth == other.binwidth)
        err2 = 1.0/(self.stat**2 + self.sys**2) + \
            1.0/(other.stat**2 + other.sys**2)
        mean = (self.y/(self.stat**2 + self.sys**2) + \
            other.y/(other.stat**2+other.sys**2))/err2
        self.y = mean
        try:
            self.stat = 1.0/math.sqrt(1.0/(self.stat**2) + 1.0/(other.stat**2))
        except ZeroDivisionError:
            pass
        try:
            self.sys = 1.0/math.sqrt(1.0/(self.sys**2) + 1.0/(other.sys**2))
        except ZeroDivisionError:
            pass
    

# 1/Nevent d2 N / (2π pT dpT dy) [c2/GeV2]
mbPlusYield = [
datapoint( x=3.50e-01, xlow=3.00e-01, binwidth=1.00e-01, y=9.71e-01, stat=1.21e-02, sys=7.76e-02 ),
datapoint( x=4.50e-01, xlow=4.00e-01, binwidth=1.00e-01, y=5.32e-01, stat=7.84e-03, sys=4.26e-02 ),
datapoint( x=5.50e-01, xlow=5.00e-01, binwidth=1.00e-01, y=3.14e-01, stat=5.46e-03, sys=2.51e-02 ),
datapoint( x=6.50e-01, xlow=6.00e-01, binwidth=1.00e-01, y=1.74e-01, stat=3.72e-03, sys=1.40e-02 ),
datapoint( x=7.50e-01, xlow=7.00e-01, binwidth=1.00e-01, y=1.08e-01, stat=2.65e-03, sys=8.64e-03 ),
datapoint( x=8.50e-01, xlow=8.00e-01, binwidth=1.00e-01, y=6.42e-02, stat=1.89e-03, sys=5.14e-03 ),
datapoint( x=9.50e-01, xlow=9.00e-01, binwidth=1.00e-01, y=4.03e-02, stat=1.42e-03, sys=3.22e-03 ),
datapoint( x=1.05e+00, xlow=1.00e+00, binwidth=1.00e-01, y=2.40e-02, stat=9.94e-04, sys=1.92e-03 ),
datapoint( x=1.15e+00, xlow=1.10e+00, binwidth=1.00e-01, y=1.55e-02, stat=7.71e-04, sys=1.24e-03 ),
datapoint( x=1.30e+00, xlow=1.20e+00, binwidth=2.00e-01, y=8.19e-03, stat=3.86e-04, sys=6.55e-04 ),
datapoint( x=1.50e+00, xlow=1.40e+00, binwidth=2.00e-01, y=3.77e-03, stat=2.46e-04, sys=3.02e-04 ),
datapoint( x=1.70e+00, xlow=1.60e+00, binwidth=2.00e-01, y=1.84e-03, stat=1.96e-04, sys=1.47e-04 ),
datapoint( x=1.90e+00, xlow=1.80e+00, binwidth=2.00e-01, y=8.32e-04, stat=1.48e-04, sys=9.98e-05 ),
datapoint( x=2.25e+00, xlow=2.00e+00, binwidth=5.00e-01, y=3.65e-04, stat=5.65e-05, sys=9.12e-05 ),
datapoint( x=2.75e+00, xlow=2.50e+00, binwidth=5.00e-01, y=1.02e-04, stat=1.11e-06, sys=1.33e-05 ),
datapoint( x=3.25e+00, xlow=3.00e+00, binwidth=5.00e-01, y=2.90e-05, stat=5.41e-07, sys=3.77e-06 ),
datapoint( x=3.75e+00, xlow=3.50e+00, binwidth=5.00e-01, y=1.00e-05, stat=3.02e-07, sys=1.30e-06 ),
datapoint( x=4.25e+00, xlow=4.00e+00, binwidth=5.00e-01, y=3.93e-06, stat=1.67e-07, sys=5.11e-07 ),
datapoint( x=4.75e+00, xlow=4.50e+00, binwidth=5.00e-01, y=1.40e-06, stat=1.00e-07, sys=1.82e-07 ),
datapoint( x=5.50e+00, xlow=5.00e+00, binwidth=1.00e+00, y=4.53e-07, stat=4.01e-08, sys=5.89e-08 ),
datapoint( x=6.50e+00, xlow=6.00e+00, binwidth=1.00e+00, y=1.07e-07, stat=2.11e-08, sys=1.39e-08 ),
datapoint( x=7.50e+00, xlow=7.00e+00, binwidth=1.00e+00, y=3.27e-08, stat=1.25e-08, sys=4.25e-09 ),
datapoint( x=9.00e+00, xlow=8.00e+00, binwidth=2.00e+00, y=7.83e-09, stat=4.58e-09, sys=1.02e-09 )
]

mbMinusYield = [
datapoint( x=3.50e-01, xlow=3.00e-01, binwidth=1.00e-01, y=9.71e-01, stat=1.22e-02, sys=7.76e-02 ),
datapoint( x=4.50e-01, xlow=4.00e-01, binwidth=1.00e-01, y=5.47e-01, stat=8.02e-03, sys=4.37e-02 ),
datapoint( x=5.50e-01, xlow=5.00e-01, binwidth=1.00e-01, y=3.09e-01, stat=5.38e-03, sys=2.47e-02 ),
datapoint( x=6.50e-01, xlow=6.00e-01, binwidth=1.00e-01, y=1.84e-01, stat=3.89e-03, sys=1.47e-02 ),
datapoint( x=7.50e-01, xlow=7.00e-01, binwidth=1.00e-01, y=1.00e-01, stat=2.50e-03, sys=8.01e-03 ),
datapoint( x=8.50e-01, xlow=8.00e-01, binwidth=1.00e-01, y=6.36e-02, stat=1.89e-03, sys=5.09e-03 ),
datapoint( x=9.50e-01, xlow=9.00e-01, binwidth=1.00e-01, y=3.80e-02, stat=1.38e-03, sys=3.04e-03 ),
datapoint( x=1.05e+00, xlow=1.00e+00, binwidth=1.00e-01, y=2.44e-02, stat=1.03e-03, sys=1.95e-03 ),
datapoint( x=1.15e+00, xlow=1.10e+00, binwidth=1.00e-01, y=1.57e-02, stat=7.87e-04, sys=1.25e-03 ),
datapoint( x=1.30e+00, xlow=1.20e+00, binwidth=2.00e-01, y=8.70e-03, stat=4.02e-04, sys=6.96e-04 ),
datapoint( x=1.50e+00, xlow=1.40e+00, binwidth=2.00e-01, y=3.62e-03, stat=2.45e-04, sys=2.90e-04 ),
datapoint( x=1.70e+00, xlow=1.60e+00, binwidth=2.00e-01, y=1.69e-03, stat=1.76e-04, sys=1.35e-04 ),
datapoint( x=1.90e+00, xlow=1.80e+00, binwidth=2.00e-01, y=1.10e-03, stat=1.59e-04, sys=1.32e-04 ),
datapoint( x=2.25e+00, xlow=2.00e+00, binwidth=5.00e-01, y=3.50e-04, stat=5.07e-05, sys=8.75e-05 ),
datapoint( x=2.75e+00, xlow=2.50e+00, binwidth=5.00e-01, y=1.00e-04, stat=1.14e-06, sys=1.32e-05 ),
datapoint( x=3.25e+00, xlow=3.00e+00, binwidth=5.00e-01, y=2.83e-05, stat=5.46e-07, sys=3.68e-06 ),
datapoint( x=3.75e+00, xlow=3.50e+00, binwidth=5.00e-01, y=9.00e-06, stat=3.01e-07, sys=1.17e-06 ),
datapoint( x=4.25e+00, xlow=4.00e+00, binwidth=5.00e-01, y=3.86e-06, stat=1.62e-07, sys=5.02e-07 ),
datapoint( x=4.75e+00, xlow=4.50e+00, binwidth=5.00e-01, y=1.50e-06, stat=1.00e-07, sys=1.95e-07 ),
datapoint( x=5.50e+00, xlow=5.00e+00, binwidth=1.00e+00, y=4.40e-07, stat=4.04e-08, sys=5.72e-08 ),
datapoint( x=6.50e+00, xlow=6.00e+00, binwidth=1.00e+00, y=1.07e-07, stat=2.10e-08, sys=1.39e-08 ),
datapoint( x=7.50e+00, xlow=7.00e+00, binwidth=1.00e+00, y=3.27e-08, stat=1.25e-08, sys=4.25e-09 ),
datapoint( x=9.00e+00, xlow=8.00e+00, binwidth=2.00e+00, y=7.83e-09, stat=4.53e-09, sys=1.02e-09 )
]

#bbcXsec = 26.1

# use NSD cross section to translate published data
bbcXsec = 30.0 # +- 3.5

mbPlusXsec = copy.deepcopy(mbPlusYield)
for elem in mbPlusXsec:
    pt = elem.x
    #corrFactor = math.sqrt(pt**2 + 0.134**2) * pt
    unitsFactor = pt * 2 * math.pi
    elem.y = elem.y * bbcXsec * unitsFactor
    # what about errors?

mbMinusXsec = copy.deepcopy(mbMinusYield)
for elem in mbMinusXsec:
    pt = elem.x
    #corrFactor = math.sqrt(pt**2 + 0.134**2)*pt
    unitsFactor = pt * 2 * math.pi
    elem.y = elem.y * bbcXsec * unitsFactor
    # what about errors?
    
# these are dsigma/dpt/deta, assuming -1 < eta < 1  INTEGRATED OVER ETA!
# (1+z) or (1-z) smockup of the charged pion fragmentation functions
# these are in picobarns
wernerPlusMock = [
datapoint( x=1.250E+00, xlow=1.000E+00, binwidth=5.000E-01, y=1.024E+10 ),
datapoint( x=1.750E+00, xlow=1.500E+00, binwidth=5.000E-01, y=1.652E+09 ),
datapoint( x=2.250E+00, xlow=2.000E+00, binwidth=5.000E-01, y=3.453E+08 ),
datapoint( x=2.750E+00, xlow=2.500E+00, binwidth=5.000E-01, y=9.600E+07 ),
datapoint( x=3.250E+00, xlow=3.000E+00, binwidth=5.000E-01, y=3.303E+07 ),
datapoint( x=3.750E+00, xlow=3.500E+00, binwidth=5.000E-01, y=1.314E+07 ),
datapoint( x=4.250E+00, xlow=4.000E+00, binwidth=5.000E-01, y=5.817E+06 ),
datapoint( x=4.750E+00, xlow=4.500E+00, binwidth=5.000E-01, y=2.823E+06 ),
datapoint( x=5.250E+00, xlow=5.000E+00, binwidth=5.000E-01, y=1.469E+06 ),
datapoint( x=5.750E+00, xlow=5.500E+00, binwidth=5.000E-01, y=8.087E+05 ),
datapoint( x=6.250E+00, xlow=6.000E+00, binwidth=5.000E-01, y=4.658E+05 ),
datapoint( x=6.750E+00, xlow=6.500E+00, binwidth=5.000E-01, y=2.798E+05 ),
datapoint( x=7.250E+00, xlow=7.000E+00, binwidth=5.000E-01, y=1.741E+05 ),
datapoint( x=7.750E+00, xlow=7.500E+00, binwidth=5.000E-01, y=1.105E+05 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=6.050E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=2.808E+04 ),
datapoint( x=1.100E+01, xlow=1.000E+01, binwidth=2.000E+00, y=1.059E+04 )
]

werner_plus_dss_cteqm5_pt = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=6.2850E+09 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.2300E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=2.3790E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=4.4530E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=1.1540E+06 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=3.7010E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=1.3830E+05 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=5.7730E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=2.6200E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.2770E+04 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=6.5710E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=3.5310E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.9710E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=1.1380E+03 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=6.7670E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=4.1030E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=2.5280E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.5930E+02 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=1.0220E+02 )
]

werner_plus_dss_cteqm5_2pt = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=3.0820E+09 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=1.0910E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=1.2460E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=2.4440E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=6.5390E+05 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=2.1420E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=8.1520E+04 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=3.4370E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=1.5800E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=7.7550E+03 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=4.0220E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=2.1720E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.2200E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=7.0860E+02 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=4.1870E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=2.5490E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.5770E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=9.9000E+01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=6.3330E+01 )
]

werner_plus_dss_cteqm5_05pt = [
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=6.2070E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=4.9310E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=8.0350E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=1.9310E+06 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=5.8490E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=2.1170E+05 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=8.6240E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=3.8500E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.8620E+04 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=9.5360E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=5.1060E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=2.8530E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=1.6420E+03 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=9.7140E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=5.9020E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=3.6630E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=2.3100E+02 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=1.4840E+02 )
]

wernerMinusMock = [
datapoint( x=1.250E+00, xlow=1.000E+00, binwidth=5.000E-01, y=1.015E+10 ),
datapoint( x=1.750E+00, xlow=1.500E+00, binwidth=5.000E-01, y=1.631E+09 ),
datapoint( x=2.250E+00, xlow=2.000E+00, binwidth=5.000E-01, y=3.386E+08 ),
datapoint( x=2.750E+00, xlow=2.500E+00, binwidth=5.000E-01, y=9.337E+07 ),
datapoint( x=3.250E+00, xlow=3.000E+00, binwidth=5.000E-01, y=3.181E+07 ),
datapoint( x=3.750E+00, xlow=3.500E+00, binwidth=5.000E-01, y=1.252E+07 ),
datapoint( x=4.250E+00, xlow=4.000E+00, binwidth=5.000E-01, y=5.476E+06 ),
datapoint( x=4.750E+00, xlow=4.500E+00, binwidth=5.000E-01, y=2.624E+06 ),
datapoint( x=5.250E+00, xlow=5.000E+00, binwidth=5.000E-01, y=1.347E+06 ),
datapoint( x=5.750E+00, xlow=5.500E+00, binwidth=5.000E-01, y=7.310E+05 ),
datapoint( x=6.250E+00, xlow=6.000E+00, binwidth=5.000E-01, y=4.150E+05 ),
datapoint( x=6.750E+00, xlow=6.500E+00, binwidth=5.000E-01, y=2.455E+05 ),
datapoint( x=7.250E+00, xlow=7.000E+00, binwidth=5.000E-01, y=1.504E+05 ),
datapoint( x=7.750E+00, xlow=7.500E+00, binwidth=5.000E-01, y=9.397E+04 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=5.036E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=2.264E+04 ),
datapoint( x=1.100E+01, xlow=1.000E+01, binwidth=2.000E+00, y=8.182E+03 )
]

werner_minus_dss_cteqm5_pt = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=6.2420E+09 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.1970E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=2.3150E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=4.2630E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=1.0840E+06 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=3.4030E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=1.2430E+05 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=5.0650E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=2.2420E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.0650E+04 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=5.3400E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=2.7950E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.5190E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=8.5440E+02 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=4.9500E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=2.9230E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.7540E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.0770E+02 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=6.7350E+01 )
]

werner_minus_dss_cteqm5_2pt = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=3.0600E+09 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=1.0710E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=1.2040E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=2.3160E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=6.0610E+05 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=1.9380E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=7.1900E+04 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=2.9520E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=1.3210E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=6.3070E+03 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=3.1820E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=1.6710E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=9.1360E+02 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=5.1620E+02 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=2.9670E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=1.7570E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.0590E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=6.4720E+01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=4.0310E+01 )
]

werner_minus_dss_cteqm5_05pt = [
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=6.1340E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=4.8210E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=7.7450E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=1.8310E+06 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=5.4390E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=1.9280E+05 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=7.6830E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=3.3510E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.5820E+04 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=7.9070E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=4.1260E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=2.2490E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=1.2620E+03 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=7.2810E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=4.3150E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=2.6120E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.6070E+02 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=1.0070E+02 )
]

werner_zero_dss_cteqm5_pt = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=6.240E+09 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.225E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=2.352E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=4.352E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=1.118E+06 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=3.555E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=1.314E+05 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=5.412E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=2.431E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.170E+04 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=5.969E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=3.170E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.746E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=9.978E+02 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=5.849E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=3.502E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=2.142E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.332E+02 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=8.455E+01 )
]

werner_zero_dss_cteqm5_2pt = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=3.071E+09 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=1.085E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=1.229E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=2.378E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=6.301E+05 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=2.041E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=7.658E+04 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=3.196E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=1.449E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=7.038E+03 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=3.603E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=1.923E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.066E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=6.102E+02 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=3.578E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=2.146E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.316E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=8.188E+01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=5.184E+01 )
]

werner_zero_dss_cteqm5_05pt = [
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=6.165E+08 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=4.911E+07 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=7.924E+06 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=1.875E+06 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=5.664E+05 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=2.021E+05 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=8.157E+04 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=3.605E+04 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.721E+04 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=8.712E+03 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=4.618E+03 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=2.548E+03 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=1.454E+03 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=8.526E+02 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=5.122E+02 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=3.137E+02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.962E+02 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=1.242E+02 )
]

class minibulb:
    """
    simple struct to store prescale and event counts for a given runnumber, 
    trigId combo
    """
    def __init__(self):
        self.prescale               = 0.0
        self.triggers               = 0
        self.triggersWithVertex     = 0
        self.triggersWithGoodVertex = 0
    


class lightbulb:
    def __init__(self, minBiasId, vertexCut = 60, sigmaBBC = 26.1):
        """
        class responsible for calculation of sampled integrated luminosities
        """
        self.vertexCut  = vertexCut
        self.sigmaBBC   = sigmaBBC
        self.minBiasId  = minBiasId
        
        self.data       = { self.minBiasId : { } }
    
    
    def addTrigger(self, trigId): self.data[trigId] = { }
    
    def processEvent(self, ev):
        for trigId in self.data.keys():
            trig    = ev.trigger(trigId)
            header  = ev.trigHeader(trigId)
            vertex  = ev.bestVert()
            
            if trig is not None and trig.didFire() > 0:
                try: mini = self.data[trigId][ev.runId()]
                except KeyError:
                    self.data[trigId][ev.runId()] = minibulb()
                    mini = self.data[trigId][ev.runId()]
                
                if mini.prescale == 0: mini.prescale = header.prescale
                elif mini.prescale != header.prescale:
                    print 'failed prescale sanity check: current event = %d, saved = %d' % (header.prescale, mini.prescale)
                
                mini.triggers += 1
                                
                if vertex is not None:
                    mini.triggersWithVertex += 1
                    if math.fabs(vertex.position()[2]) < self.vertexCut: 
                        mini.triggersWithGoodVertex += 1
                
    
    
    def vertexEfficiency(self, trigId, runList = None):
        """vertex efficiency for trigId integrated over runList"""
        t = self.data[trigId]
        triggers = 0
        triggersWithVertex = 0
        for run in t.keys():
            if runList is None or run in runList:
                triggers += t[run].triggers
                triggersWithVertex += t[run].triggersWithVertex
        
        return float(triggersWithVertex) / triggers
    
    
    def vertexPosition(self, trigId, runList = None):
        """fraction of events with reco vertex that pass cut for trigId in runList"""
        t = self.data[trigId]
        triggersWithVertex = 0
        triggersWithGoodVertex = 0
        for run in t.keys():
            if runList is None or run in runList:
                triggersWithVertex += t[run].triggersWithVertex
                triggersWithGoodVertex += t[run].triggersWithGoodVertex
        
        return float(triggersWithGoodVertex) / triggersWithVertex
    
    
    def prescaleFactor(self, trigId, runList = None):
        """some options for calculating prescale factors include
        a)  mean of (ps_mb / ps_trig) weighted by # triggered events in run 
        b)  sum(ps_mb * n_mb) / sum(ps_ht * n_ht) where sum is taken over all runs with trigger active
        current implementation is b)"""
        
        triggerBulbs = self.data[trigId]
        minBiasBulbs = self.data[self.minBiasId]
        
        sum_mb = 0
        sum_tr = 0
        
        for run in triggerBulbs.keys():
            if runList is None or run in runList:
                try:
                    mb = minBiasBulbs[run]
                except KeyError:
                    print 'no minbias data for %d' % (run,)
                    continue
                
                tr = triggerBulbs[run]
                
                sum_mb += (mb.prescale * mb.triggers)
                sum_tr += (tr.prescale * tr.triggers)
        
        if sum_tr > 0: return float(sum_mb) / sum_tr
        return 0.
    
    
    def sampledLuminosity(self, trigId, runList = None):
        """calculate analysis-worthy sampled luminosity for trigId in runList"""
        triggersWithGoodVertex  = 0
        
        mb = self.data[self.minBiasId]
        tr = self.data[trigId]
        
        if runList is not None: union = [run for run in tr.keys() if run in runList]
        else: union = tr.keys()
            
        for run in union:
            try: triggersWithGoodVertex += mb[run].triggersWithGoodVertex
            except KeyError: pass
        
        vtx_eff = self.vertexEfficiency(self.minBiasId, union)
        
        mbSampled = triggersWithGoodVertex / (vtx_eff * self.sigmaBBC)
        return mbSampled * self.prescaleFactor(trigId, union)
    
    
    def integratedLuminosity(self, trigId, runList = None):
        """calculate raw integrated luminosity for trigId in runList"""
        
        mb = self.data[self.minBiasId]
        tr = self.data[trigId]
        
        counter = 0
        
        if runList is not None: union = [run for run in tr.keys() if run in runList]
        else: union = tr.keys()
        
        for run in union:
            try: counter += mb[run].triggers * mb[run].prescale
            except KeyError: pass
        
        return counter / self.sigmaBBC
    
    
    def runList(self, trigId): return self.data[trigId].keys()
    
    
    def prettyPrint(self, millibarns):
        """prints a string for a given integrated luminosity in millibarns"""
        if millibarns < 10**3:
            return '%7.3f mb^-1' % (millibarns,)
        elif millibarns < 10**6:
            millibarns = millibarns / 10**3
            return '%7.3f μb^-1' % (millibarns,)
        elif millibarns < 10**9:
            millibarns = millibarns / 10**6
            return '%7.3f nb^-1' % (millibarns,)
        elif millibarns < 10**12:
            millibarns = millibarns / 10**9
            return '%7.3f pb^-1' % (millibarns,)
    
    
    def printSummary(self):
        print '================================================================\
        ==========================='
        print 'trigId    L_int_mb      ε_vtx  ε_mb   vz     vz_mb    L_samp_mb \
           < ps >      L_sampled'
        print '----------------------------------------------------------------\
        ---------------------------'
        trigList = self.data.keys()
        trigList.sort()
        for trigId in trigList:
            runList     = self.runList(trigId)
            l_int_mb    = self.integratedLuminosity(self.minBiasId, runList)
            eps_vtx     = self.vertexEfficiency(trigId, runList)
            vz          = self.vertexPosition(trigId, runList)
            eps_vtx_mb  = self.vertexEfficiency(self.minBiasId, runList)
            vz_mb       = self.vertexPosition(self.minBiasId, runList)
            l_samp_mb   = self.sampledLuminosity(self.minBiasId, runList)
            psFactor    = self.prescaleFactor(trigId, runList)
            l_samp      = self.sampledLuminosity(trigId, runList)
            print '%6d  %s   %5.3f  %5.3f  %5.3f  %5.3f  %s   %7.1f   %s' % \
                (trigId, self.prettyPrint(l_int_mb), eps_vtx, eps_vtx_mb, vz, 
                vz_mb, self.prettyPrint(l_samp_mb), psFactor, 
                self.prettyPrint(l_samp))
        print '================================================================\
        ==========================='
    


def initReader(reader):
    """configuration options for the StSpinTreeReader"""
    reader.selectDataset('/Users/kocolosk/data/run5/dataset.txt')
    
    #reader.selectRunlist('/Library/STAR/DEV/StRoot/StSpinPool/StSpinTree/filters/run6_jets.runlist')
    #reader.selectRun(6172086)
    #reader.selectRun(6160062)
    #reader.selectRun(6158041)
    #reader.selectRun(6158059)
    #reader.selectRun(6158057)
    reader.selectRunlist('/Library/STAR/DEV/StRoot/StSpinPool/StSpinTree/filters/run5_jets.runlist')
    reader.removeRun(6150005)
    
    reader.connectJets          = False
    reader.connectNeutralJets   = False
    reader.connectChargedPions  = False
    reader.connectBemcPions     = False
    reader.connectBemcElectrons = False
    reader.connectGammas        = False
    reader.connectEemcPions     = False
    
    #reader.selectTrigger(117001)
    #reader.selectTrigger(137221)
    #reader.selectTrigger(137222)
    #reader.selectTrigger(137611)
    #reader.selectTrigger(137622)
    
    reader.selectTrigger(96011)
    reader.selectTrigger(96201)
    reader.selectTrigger(96211)
    reader.selectTrigger(96221)
    reader.selectTrigger(96233)
    
    
    reader.requireDidFire       = True
    reader.requireShouldFire    = False

# Some notes about Spectra's cross section
# Figure 2 in nucl-ex/0601033 plots charge-separated invariant yields as 

# (1/Nevents) * 1/(2pi*pt) * d^2/dptdy (N_pi)

# well, sigma_BBC == N_ev/L, so if you average these yields and multiply by 
# sigma_BBC * E/pT you should get the cross section in Figure 6 of that paper.  
# Next step is to make comparisons to Marco's new theory predictions

def plotOldYields():
    graphPlus = ROOT.TGraphErrors(len(mbPlusYield))
    for row,elem in enumerate(mbPlusYield):
        graphPlus.SetPoint(row, elem.x, elem.y)
        graphPlus.SetPointError(row, elem.binwidth/2, math.sqrt(elem.stat**2 + elem.sys**2))
    
    graphMinus = ROOT.TGraphErrors(len(mbMinusYield))
    for row,elem in enumerate(mbMinusYield):
        graphMinus.SetPoint(row, elem.x, elem.y)
        graphMinus.SetPointError(row, elem.binwidth/2, math.sqrt(elem.stat**2 + elem.sys**2))
    
    bbcXsec = 26.1 # mb
    graphXsec = ROOT.TGraphErrors(len(mbPlusYield))
    for row in range(graphXsec.GetN()):
        rawAvg = (mbPlusYield[row].y + mbMinusYield[row].y)/2
        pt = mbPlusYield[row].x
        corrFactor = math.sqrt(pt**2 + 0.134**2)/pt
        graphXsec.SetPoint(row, mbPlusYield[row].x, rawAvg * bbcXsec )
        print '%1.3f %e %f' % (mbPlusYield[row].x, rawAvg*bbcXsec, corrFactor)
        graphXsec.SetPointError(row, mbMinusYield[row].binwidth/2, 0.0)
    
    
    c1 = ROOT.TCanvas()
    graphPlus.SetMarkerStyle(20)
    graphPlus.Draw('ap')
    
    graphMinus.SetMarkerStyle(21)
    graphMinus.Draw('p same')
    
    
    c2 = ROOT.TCanvas('xsec')
    graphXsec.SetMarkerStyle(30)
    graphXsec.Draw('ap')
    
    raw_input('press enter:')


def comboPlot(dataPoints, theoryPoints=None):
    c = ROOT.TCanvas('blah', 'blah', 650, 800)
    
    pad1 = ROOT.TPad('pad1', 'xsec', 0, 0.45, 1.0, 1.0)
    pad1.SetBottomMargin(0)
    pad1.SetTopMargin(0.02)
    
    pad2 = ROOT.TPad('pad2', 'errors', 0, 0.28, 1.0, 0.45)
    pad2.SetBottomMargin(0)
    pad2.SetTopMargin(0)
    
    pad3 = ROOT.TPad('pad3', 'nlo ratio', 0, 0, 1.0, 0.28)
    pad3.SetBottomMargin(0.22)
    pad3.SetTopMargin(0)
    
    pad1.Draw()
    pad2.Draw()
    pad3.Draw()
    
    histos = []
    
    hX = ROOT.TH2F('hX', 'hX', 2, 0, 12, 2, 0.000000011, 50.)
    hX.SetXTitle('p_{t} [GeV/c]')
    hX.SetYTitle('Ed^{3}#sigma/dp^{3} [mb GeV^{-2}c^{3}]')
    histos.append(hX)
    
    hE = ROOT.TH2F('hE', 'hE', 2, 0, 12, 2, -2.0, 2.0)
    hE.SetYTitle('#Delta#sigma/#sigma (stat)')
    hE.GetYaxis().SetNdivisions(505)
    histos.append(hE)
    
    hR = ROOT.TH2F('hR', 'hR', 2, 0, 12, 2, -1., 1.)
    hR.SetXTitle('p_{t} [GeV/c]')
    hR.SetYTitle('(data-NLO)/NLO')
    hR.GetXaxis().SetLabelFont(63)
    hR.GetXaxis().SetLabelSize(14)
    hR.GetXaxis().SetTitleFont(63)
    hR.GetXaxis().SetTitleSize(16)
    hR.GetXaxis().SetTitleOffset(4)
    hR.GetXaxis().SetNdivisions(510)
    histos.append(hR)
    
    for h in histos:
        h.GetYaxis().SetLabelFont(63)
        h.GetYaxis().SetLabelSize(14)
        h.GetYaxis().SetTitleFont(63)
        h.GetYaxis().SetTitleSize(16)
        h.GetYaxis().SetTitleOffset(2.5)
        
    pad1.SetLogy()
    pad1.cd()
    hX.Draw()
    
    dataGraphX = ROOT.TGraphErrors(len(dataPoints))
    for row,elem in enumerate(dataPoints):
        dataGraphX.SetPoint(row, elem.x, elem.y)
        dataGraphX.SetPointError(row,0.0,math.sqrt(elem.stat**2 + elem.sys**2))
    dataGraphX.Draw('P')
    
    theoryGraph = ROOT.TGraph(len(theoryPoints))
    for row,elem in enumerate(theoryPoints):
        theoryGraph.SetPoint(row, elem.x, elem.y * 10**-9 / (2* math.pi))
    theoryGraph.Draw('L')
    # draw datapoints and theory
    #[gr.Draw('P') for gr in dataGraphsStatSys]
    #theoryGraphs[0].Draw('L')
    
    legX = ROOT.TLegend(0.55, 0.65, 0.85, 0.85)
    legX.SetBorderSize(0)
    legX.SetFillColor(10)
    legX.SetTextFont(43)
    legX.SetTextSize(16)
    
    # add legend entries
    
    legX.Draw()
    
    # end of pad 1
    
    pad2.cd()
    pad2.SetLogy(0)
    hE.Draw()
    
    # ok, now we do data-tho
    
    lineE = ROOT.TLine(0, 0, 12, 0)
    lineE.Draw()
    
    #for gr in dataGraphsStatSys:
    #    gr.Draw('LF')
        
    
    legE = ROOT.TLegend(0.35, 0.67, 0.65, 0.95)
    legE.SetBorderSize(0)
    legE.SetFillColor(10)
    legE.SetTextFont(43)
    legE.SetTextSize(14)
    legE.SetMargin(0.15)
    
    # add legend entries
    
    legE.Draw()
    
    tb = ROOT.TText(0.2, 0.8, 'b)')
    tb.SetNDC()
    tb.SetTextFont(63)
    tb.SetTextSize(18)
    tb.Draw()
    
    # end of pad 2
    
    pad3.cd()
    hR.Draw()
    
    lineR = ROOT.TLine(0, 0, 12, 0)
    lineR.Draw()
    
    # draw ratio to NLO
    
    legR1 = ROOT.TLegend(0.66, 0.68, 0.88, 0.97)
    legR1.SetBorderSize(0)
    legR1.SetFillColor(10)
    legR1.SetTextFont(43)
    legR1.SetTextSize(14)
    
    # add entries to legend1
    
    legR1.Draw()
    
    legR2 = ROOT.TLegend(0.44, 0.76, 0.66, 0.97)
    legR2.SetBorderSize(0)
    legR2.SetFillColor(10)
    legR2.SetTextFont(43)
    legR2.SetTextSize(14)
    legR2.SetMargin(0.2)
    
    # add more entries to legend 2
    
    legR2.Draw()
    
    tc = ROOT.TText(0.2, 0.85, 'c)')
    tc.SetNDC()
    tc.SetTextFont(63)
    tc.SetTextSize(18)
    tc.Draw()
    
    raw_input('press enter:')
    
    return c


def simpleXsecPlot(dataPoints, theoryPoints=None):
    dataGraph1 = ROOT.TGraphErrors(len(dataPoints))
    for row,elem in enumerate(dataPoints):
        dataGraph1.SetPoint(row, elem.x, elem.y)
        dataGraph1.SetPointError(row,0.0,math.sqrt(elem.stat**2 + elem.sys**2))
        
    c1 = ROOT.TCanvas('c1')
    c1.SetLogy()
    
    dataGraph1.SetTitle('#pi^{-} xsec from published yields')
    dataGraph1.SetMarkerStyle(21)
    dataGraph1.GetXaxis().SetTitle('p_{t}')
    dataGraph1.GetYaxis().SetTitle('d^{2}#sigma/(dptd#eta) (mb/GeV)')
    dataGraph1.GetYaxis().SetRangeUser(10**-5, 10)
    dataGraph1.Draw('ap')
    
    #c1.Print('xsec_minus_mock.gif')
    
    theoryGraph = ROOT.TGraph(len(theoryPoints))
    for row,elem in enumerate(theoryPoints):
        rescaleFactor = 10**-9 / (2)
        theoryGraph.SetPoint(row, elem.x, elem.y * rescaleFactor)
    theoryGraph.Draw('l')
    
    dataGraph2 = ROOT.TGraphErrors(len(dataPoints))
    for row,elem in enumerate(dataPoints):
        dataGraph2.SetPoint(row, elem.x, elem.y / theoryGraph.Eval(elem.x))
        dataGraph1.SetPointError(row,0.0,math.sqrt(elem.stat**2 + elem.sys**2))
        
    
    dataGraph2.SetTitle('Data/Theory Ratio for #pi^{-}')
    dataGraph2.SetMarkerStyle(21)
    dataGraph2.GetXaxis().SetTitle('p_{t}')
    dataGraph2.GetYaxis().SetTitle('data/theory')    
    dataGraph2.GetYaxis().SetRangeUser(0,2)
    
    c2 = ROOT.TCanvas('c2')
    c2.SetLogy(0)
    dataGraph2.Draw('ap')
    #c2.Print('xsec_minus_mock_ratio.gif')
    
    raw_input('press enter:')


def test():
    ROOT.gSystem.Load('StJetMaker')
    ROOT.gSystem.Load('StChargedPionAnalysisMaker')
    ROOT.gSystem.Load('StSpinTree')
    reader = ROOT.StSpinTreeReader()
    
    initReader(reader)
    
    ev = reader.event()
    
    #lumi = lightbulb(minBiasId = 117001)
    #lumi.addTrigger(137221)
    #lumi.addTrigger(137222)
    #lumi.addTrigger(137611)
    #lumi.addTrigger(137622)
    
    lumi = lightbulb(minBiasId = 96011)
    lumi.addTrigger(96201)
    lumi.addTrigger(96211)
    lumi.addTrigger(96221)
    lumi.addTrigger(96233)
    
    for i in range(reader.GetEntries()):
        reader.GetEntry(i)
        
        lumi.processEvent(ev)
    
    lumi.printSummary()
    
