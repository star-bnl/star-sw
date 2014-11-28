name    = __name__.split('.')[-1]
VERSION = '$Id: pt_pk.py,v 1.1 2009/04/07 16:34:38 kocolosk Exp $'[5:-2]

import ROOT
from array import array
from analysis import pid

class_ = ROOT.TH1D

binning = {
    'nbinsx': 5,
    'xbins': array('d', [2.0, 3.18, 4.56, 6.32, 8.8, 12.84])
}

props = {
    'SetXTitle': ('p_{T}',)
}

branches = ('mVertices*', 'mBbcTimeBin', 'mRunId', 'mTracks*')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu = isinstance(event, ROOT.StChargedPionMcEvent)
    bin = event.bbcTimeBin()/32
    bbc_cut = simu or bin in (7,8,9) or (event.runId() > 7000000 and bin==6)
    return vertex_cut and bbc_cut

def accept_track(event, track):
    eta_cut = abs( track.eta() ) < 1.0
    dca_cut = abs( track.globalDca().mag() ) < 1.0
    fit_cut = track.nHitsFit() > 25
    if isinstance(event, ROOT.StChargedPionMcEvent):
        pid_cut = track.geantId() in (11,12,14,15)
    else:
        nsigpi = pid.shift(event.runId(), track.nSigmaPion())
        pid_cut = nsigpi < -2.1
    return eta_cut and dca_cut and fit_cut and pid_cut

def analyze(event, **kw):
    if isinstance(event, ROOT.StChargedPionMcEvent):
        collection = event.matchedPairs()
    else:
        collection = event.tracks()
    for track in collection:
        if event.charge_filter(track) and accept_track(event, track):
            yield (track.Pt(),)

