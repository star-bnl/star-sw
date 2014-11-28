name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: x.py,v 1.1 2009/04/07 16:35:31 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TProfile

binning = {
    'nbinsx': 60,
    'xbins': (0.0, 15.0)
}

props = {
    'SetXTitle': ('MC #pi p_{T}',),
    'SetYTitle': ('<partonic p_{T}/100>',)
}

branches = ('mParton3*', 'mParton4*', 'mMcTracks*')

def accept_event(event):
    simu = isinstance(event, ROOT.StChargedPionMcEvent)
    return simu

def accept_track(track):
    eta_cut = abs(track.etaMc()) < 1.0
    pid_cut = track.geantId() in (8,9)
    return eta_cut and pid_cut

def analyze(event, **kw):
    XYZVector = ROOT.Math.XYZVector
    DeltaR = ROOT.Math.VectorUtil.DeltaR
    p3 = XYZVector(event.parton3().Vect())
    p4 = XYZVector(event.parton4().Vect())
    for track in filter(event.charge_filter, event.mcTracks()):
        if accept_track(track):
            vec = XYZVector(track.pxMc(), track.pyMc(), track.pzMc())
            if DeltaR(vec, p3) <  DeltaR(vec, p4):
                yield (track.ptMc(), event.parton3().Pt()/100)
            else:
                yield (track.ptMc(), event.parton4().Pt()/100)

