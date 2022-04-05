name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: raw.py,v 1.1 2009/04/07 16:34:52 kocolosk Exp $'[5:-2]

from array import array
import ROOT

class_ = ROOT.TH2D

binning = {
    'nbinsx': 5,
    'xbins': array('d', [2.0, 3.18,4.56, 6.32, 8.80, 12.84]),
    'nbinsy': 200,
    'ybins': (0.0, 50.0)
}

props = {
    'SetXTitle': ('#pi p_{T}',), 
    'SetYTitle': ('partonic p_{T}',)
}

branches = ('mMcVertex*', 'mPythiaRecord*', 'mHardP')

def accept_event(event):
    simu_cut = isinstance(event, ROOT.StChargedPionMcEvent)
    vz_cut = (abs(event.mcVertex().z()) < 60)
    return simu_cut and vz_cut


def accept_track(track):
    etamc_cut = abs(track.vec.Eta()) < 1.0
    pid_cut = abs(track.id) == 211
    return etamc_cut and pid_cut


def analyze(event, **kw):
    for track in event.pythiaRecord():
        if event.charge_filter(track) and accept_track(track):
            yield (track.vec.Pt(), event.hardP())

