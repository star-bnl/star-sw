name    = '_'.join(__name__.split('.')[-2:])
VERSION = '$Id: mcz.py,v 1.1 2009/04/07 16:35:31 kocolosk Exp $'[5:-2]

import ROOT

class_ = ROOT.TProfile

binning = {
    'nbinsx': 60,
    'xbins': (0.0, 15.0)
}

props = {
    'SetXTitle': ('MC #pi p_{T}',),
    'SetYTitle': ('<MC #pi p_{T} / particle jet p_{T}>',)
}

branches = ('mMcJets*', 'mMcTracks*')

vec = ROOT.TVector3()

def accept_event(event):
    simu = isinstance(event, ROOT.StChargedPionMcEvent)
    return simu

def accept_track(track):
    eta_cut = abs(track.etaMc()) < 1.0
    pid_cut = track.geantId() in (8,9)
    return eta_cut and pid_cut

def analyze(event, **kw):
    for track in filter(event.charge_filter, event.mcTracks()):
        if accept_track(track):
            vec.SetXYZ(track.pxMc(), track.pyMc(), track.pzMc())
            jets = map(lambda j: (j, vec.DeltaR(j.Vect())), event.mcJets())
            if jets:
                result = reduce(lambda (j1, dR1), (j2, dR2): \
                    dR1 < dR2 and (j1, dR1) or (j2, dR2), jets)
                yield (track.ptMc(), track.ptMc() / result[0].Pt())

