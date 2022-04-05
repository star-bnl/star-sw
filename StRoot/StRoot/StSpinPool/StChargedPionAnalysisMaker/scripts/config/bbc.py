name    = __name__.split('.')[-1]
VERSION = '$Id: bbc.py,v 1.1 2009/04/07 16:34:37 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH1D

binning = {
    'nbinsx': 400,
    'xbins': (-0.5, 399.5)
}

props = {
    'SetXTitle': (name,)
}

branches = ('mBbcTimeBin', )

def accept_event(event):
    return True

def analyze(event, **kw):
    yield (event.bbcTimeBin(),)

