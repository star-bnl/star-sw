name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: denom.py,v 1.1 2009/04/07 16:34:58 kocolosk Exp $'[5:-2]

from array import array
import ROOT
import mcasym

class_ = ROOT.TH1D

binning = {
    'nbinsx': 5,
    'xbins': array('d', [2.00, 3.18, 4.56, 6.32, 8.80, 12.84])
}

props = {
    'SetXTitle': ('p_{T}',),
    'SetYTitle': (name,)
}

branches = ('mFlavor*', 'mX1', 'mParton1*', 'mParton2*', 'mParton3*',  
    'mProcessId', 'mVertices*', 'mMatchedPairs*')

def accept_event(event):
    vertex_cut = event.nVertices() > 0
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    return vertex_cut and simu_cut

def accept_track(track):
    eta_cut = abs( track.eta() ) < 1.0
    dca_cut = abs( track.globalDca().mag() ) < 1.0
    fit_cut = track.nHitsFit() > 25
    pid_cut = track.geantId() in (8,9)
    return eta_cut and dca_cut and fit_cut and pid_cut

def analyze(event, **kw):
    for track in event.matchedPairs():
        if event.charge_filter(track) and accept_track(track):
            yield (track.pt(), mcasym.denom('NLO', event))

