name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: M090.py,v 1.1 2009/04/07 16:35:03 kocolosk Exp $'[5:-2]

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
    'mProcessId', 'mMcVertex*', 'mPythiaRecord*')

def accept_event(event):
    vertex_cut = abs(event.mcVertex().z()) < 60
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    return vertex_cut and simu_cut

def accept_track(track):
    etamc_cut = abs(track.vec.Eta()) < 1.0
    pid_cut = abs(track.id) == 211
    return etamc_cut and pid_cut

def analyze(event, **kw):
    for track in event.pythiaRecord():
        if event.charge_filter(track) and accept_track(track):
            yield (track.vec.Pt(), mcasym.num('M090', event))

