name    = __name__.split('.')[-1]
VERSION = '$Id: pt_p_nSigmaPion.py,v 1.1 2009/04/07 16:34:38 kocolosk Exp $'[5:-2]

import ROOT
from array import array
from analysis import pid

class_ = ROOT.TH3D

binning = {
    'nbinsx': 5,
    'xbins': array('d', [2.0, 3.18, 4.56, 6.32, 8.8, 12.84]),
    'nbinsy': 36,
    'ybins': array('d', [2.0+0.5*i for i in range(37)]),
    'nbinsz': 480,
    'zbins': array('d', [-12.0+0.05*i for i in range(481)])
}

props = {
    'SetXTitle': ('p_{T}',),
    'SetYTitle': ('momentum',),
    'SetZTitle': ('n#sigma(#pi) + 6*track.charge()',)
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

def analyze(event, jet_trigger_filter, **kw):
    for track in event.tracks():
        if event.charge_filter(track) and accept_track(event, track):
            nsigpi = pid.shift(event.runId(), track.nSigmaPion())
            yield (track.Pt(), track.P(), nsigpi + 6*track.charge())
