name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: STD.py,v 1.1 2009/04/07 16:35:28 kocolosk Exp $'[5:-2]

from array import array
import ROOT
import mcasym

class_ = ROOT.TH1D

binning = {
    'nbinsx': 7,
    'xbins': array('d', [0.0, 0.075, 0.125, 0.2, 0.3, 0.45, 0.65, 1.0])
}

props = {
    'SetXTitle': ('p_{T}',),
    'SetYTitle': (name,)
}

branches = ('mFlavor*', 'mX1', 'mParton1*', 'mParton2*', 'mParton3*',  
    'mProcessId', 'mVertices*', 'mMatchedPairs*', 'mJets*', 'mHighTowers*', 
    'mJetPatches*')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    return vertex_cut and simu_cut

def accept_jet(event, jet):
    pt_cut = 10.0 < jet.Pt() < 30.0
    if event.year == 2005:
        eta_cut = 0.2 < jet.detectorEta() < 0.8
        rt_cut = 0.1 < (jet.tpcEtSum() / jet.Et()) < 0.9
    else:
        eta_cut = -0.7 < jet.detectorEta() < 0.9
        rt_cut = (jet.tpcEtSum() / jet.Et()) > 0.08
    return eta_cut and rt_cut and pt_cut

def accept_track(track):
    eta_cut = abs( track.eta() ) < 1.0
    dca_cut = abs( track.globalDca().mag() ) < 1.0
    fit_cut = track.nHitsFit() > 25
    pid_cut = track.geantId() in (8,9)
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
            for track in filter(event.charge_filter, event.matchedPairs()):
                if accept_track(track) and abs(track.DeltaPhi(jet))>2.0:
                    z = track.ptPr()/shifted(event.year, jet.Pt())
                    yield (z, mcasym.num('MIN', event))

