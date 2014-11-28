name    = __name__.split('.')[-1]
VERSION = '$Id: ptMc_ptPr.py,v 1.1 2009/04/07 16:34:37 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH2D

binning = {
    'nbinsx': 200,
    'xbins': (0.0, 20.0),
    'nbinsy': 200,
    'ybins': (0.0, 20.0)
}

props = {
    'SetXTitle': ('ptMc',),
    'SetYTitle': ('ptPr',)
}

branches = ('mVertices*', 'mMatchedPairs*')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    return vertex_cut and simu_cut

def accept_track(track):
    pid_cut = track.geantId() in (8,9)
    return pid_cut

def analyze(event, **kw):
    for track in event.matchedPairs():
        if event.charge_filter(track) and accept_track(track):
            yield (track.ptMc(), track.ptPr())

