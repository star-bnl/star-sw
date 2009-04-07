name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: nSigmaPion.py,v 1.1 2009/04/07 16:34:46 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH1D

binning = {
    'nbinsx': 240,
    'xbins': (-6.0, 6.0)
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
    return eta_cut and dca_cut and fit_cut

def analyze(event, jet_trigger_filter, **kw):
    for jet in event.jets():
        if accept_jet(event, jet) and jet_trigger_filter(event, jet):
            for track in filter(event.charge_filter, event.tracks()):
                if accept_track(event,track) and abs(track.DeltaPhi(jet))>2.0:
                    yield (track.nSigmaPion(),)

