name    = __name__.split('.')[-1]
VERSION = '$Id: nSigmaPion.py,v 1.1 2009/04/07 16:34:37 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH1D

binning = {
    'nbinsx': 240,
    'xbins': (-6.0, 6.0)
}

props = {
    'SetXTitle': ('n#sigma(#pi)',)
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
    return eta_cut and dca_cut and fit_cut

def analyze(event, **kw):
    for track in event.tracks():
        if event.charge_filter(track) and accept_track(event, track):
            yield (track.nSigmaPion(),)

