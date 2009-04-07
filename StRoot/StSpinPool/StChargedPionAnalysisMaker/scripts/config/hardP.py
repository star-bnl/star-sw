name    = __name__.split('.')[-1]
VERSION = '$Id: hardP.py,v 1.1 2009/04/07 16:34:37 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH1D

binning = {
    'nbinsx': 280,
    'xbins': (0.0, 70.0)
}

props = {
    'SetXTitle': ('partonic p_{T}',)
}

branches = ('mVertices*', 'mHardP')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    return vertex_cut and simu_cut

def analyze(event, **kw):
    yield (event.hardP(),)

