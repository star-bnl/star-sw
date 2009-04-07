name    = __name__.split('.')[-1]
VERSION = '$Id: vzBBC.py,v 1.1 2009/04/07 16:34:38 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH1D

binning = {
    'nbinsx': 500,
    'xbins': (-150., 150.)
}

props = {
    'SetXTitle': (name,)
}

branches = ('mVertices*', 'mBbcTimeBin', 'mRunId')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu = isinstance(event, ROOT.StChargedPionMcEvent)
    bin = event.bbcTimeBin()/32
    bbc_cut = simu or bin in (7,8,9) or (event.runId() > 7000000 and bin==6)
    return vertex_cut and bbc_cut

def analyze(event, **kw):
    yield (event.vertex(0).z(),)

