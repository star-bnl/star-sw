name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: jetpt.py,v 1.1 2009/04/07 16:35:31 kocolosk Exp $'[5:-2]

import ROOT
from analysis import pid

class_ = ROOT.TProfile

binning = {
    'nbinsx': 60,
    'xbins': (0.0, 15.0)
}

props = {
    'SetXTitle': ('#pi p_{T}',),
    'SetYTitle': ('<jet p_{T}>',)
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
    if event.year == 2005:
        eta_cut = 0.2 < jet.detectorEta() < 0.8
        rt_cut = 0.1 < (jet.tpcEtSum() / jet.Et()) < 0.9
    else:
        eta_cut = -0.7 < jet.detectorEta() < 0.9
        rt_cut = (jet.tpcEtSum() / jet.Et()) > 0.08
    return eta_cut and rt_cut

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

def analyze(event, jet_trigger_filter, **kw):
    good_jets = filter(lambda j: accept_jet(event, j) and \
        jet_trigger_filter(event, j), event.jets())
    good_tracks = filter(lambda t: event.charge_filter(t) and \
        accept_track(event, t), event.tracks())
    for track in good_tracks:
        jets = map(lambda j: (j, track.DeltaR(j)), good_jets)
        if jets:
            result = reduce(lambda (j1, dR1), (j2, dR2): \
                dR1 < dR2 and (j1, dR1) or (j2, dR2), jets)
            yield (track.Pt(), result[0].Pt())

