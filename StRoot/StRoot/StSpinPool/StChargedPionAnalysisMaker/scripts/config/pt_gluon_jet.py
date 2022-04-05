name    = __name__.split('.')[-1]
VERSION = '$Id: pt_gluon_jet.py,v 1.1 2009/04/07 16:34:38 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TH1D

binning = {
    'nbinsx': 40,
    'xbins': (0.0, 20.0)
}

props = {
    'SetXTitle': (name,)
}

branches = ('mMcVertex*', 'mVertices*', 'mMatchedPairs*', 'mFlavor*', 
    'mParton3*', 'mParton4*')

def accept_event(event):
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    vz_cut = abs(event.mcVertex().z()) < 60
    vertex_cut = event.nVertices() > 0
    return simu_cut and vz_cut and vertex_cut

def accept_track(event, track):
    etamc_cut = abs(track.etaMc()) < 1.0
    eta_cut = abs( track.eta() ) < 1.0
    dca_cut = abs( track.globalDca().mag() ) < 1.0
    fit_cut = track.nHitsFit() > 25
    pid_cut = track.geantId() in (8,9)
    return etamc_cut and eta_cut and dca_cut and fit_cut and pid_cut

def analyze(event, **kw):
    XYZVector = ROOT.Math.XYZVector
    DeltaR = ROOT.Math.VectorUtil.DeltaR
    p3 = XYZVector(event.parton3().Vect())
    p4 = XYZVector(event.parton4().Vect())
    for track in filter(event.charge_filter, event.matchedPairs()):
        if accept_track(event, track):
            vec = XYZVector(track.pxPr(), track.pyPr(), track.pzPr())
            if DeltaR(vec, p3) <  DeltaR(vec, p4):
                if event.flavor(3) == 21:
                    yield (track.ptPr(), )
            else:
                if event.flavor(4) == 21:
                    yield (track.ptPr(), )

