name    = __name__.split('.')[-1]
VERSION = '$Id: meanpt.py,v 1.1 2009/04/07 16:34:37 kocolosk Exp $'[5:-2]

import ROOT
from array import array
from analysis import pid

class_ = ROOT.TProfile

binning = {
    'nbinsx': 7,
    'xbins': array('d', [0.0, 0.075, 0.125, 0.2, 0.3, 0.45, 0.65, 1.0])
}

props = {
    'SetXTitle': (name,)
}

branches = ('mVertices*', 'mBbcTimeBin', 'mRunId', 'mJets*', 'mTracks*',
    'mHighTowers*', 'mJetPatches*')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu = isinstance(event, ROOT.StChargedPionMcEvent)
    bin = event.bbcTimeBin()/32
    bbc_cut = simu or bin in (7,8,9) or (event.runId() > 7000000 and bin==6)
    return vertex_cut and bbc_cut

def accept_jet(event, jet):
    pt_cut = 10.0 < jet.Pt() < 30.0
    if event.year == 2005:
        eta_cut = 0.2 < jet.detectorEta() < 0.8
        rt_cut = 0.1 < (jet.tpcEtSum() / jet.Et()) < 0.9
    else:
        eta_cut = -0.7 < jet.detectorEta() < 0.9
        rt_cut = (jet.tpcEtSum() / jet.Et()) > 0.08
    return eta_cut and rt_cut and pt_cut

def accept_track(event, track):
    eta_cut = abs( track.eta() ) < 1.0
    dca_cut = abs( track.globalDca().mag() ) < 1.0
    fit_cut = track.nHitsFit() > 25
    if isinstance(event, ROOT.StChargedPionMcEvent):
        pid_cut = True
    else:
        pid_min = pid.min(event.runId())
        pid_max = pid.max(event.runId())
        pid_cut = pid_min < track.nSigmaPion() < pid_max
    return eta_cut and dca_cut and fit_cut and pid_cut

def shifted(year, jetpt):
    """applies pT shift to correct measured jet pT back to particle level"""
    if year == 2006:
        ## /STAR/node/12022 for details
        return 1.538 + 0.8439*jetpt - 0.001691*jetpt*jetpt
    else:
        return jetpt

def analyze(event, jet_trigger_filter, **kw):
    for jet in event.jets():
        if accept_jet(event, jet) and jet_trigger_filter(event, jet):
            for track in filter(event.charge_filter, event.tracks()):
                if accept_track(event,track) and abs(track.DeltaPhi(jet))>2.0:
                    z = track.Pt() / shifted(event.year, jet.Pt())
                    yield (z, track.Pt())

