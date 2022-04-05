name    = __name__.split('.')[-1]
VERSION = '$Id: cosTheta.py,v 1.1 2009/04/07 16:34:37 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH1D

binning = {
    'nbinsx': 200,
    'xbins': (-1.0, 1.0)
}

props = {
    'SetXTitle': ('cos(#Theta)',)
}

branches = ('mVertices*', 'mParton3*')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    return vertex_cut and simu_cut

def analyze(event, **kw):
    yield (event.cosTheta(),)

