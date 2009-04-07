import math
from array import array
import ROOT
ROOT.gSystem.Load('libMySQL')
mapping = ROOT.StEmcMappingDb(20060801)


# eta_bins = [-1.0+0.05*i for i in range(41)]
# eta_bins[0]  = -0.984
# eta_bins[20] = -0.0035
# eta_bins[21] =  0.0035
# eta_bins[40] =  0.984
# eta_bins = array('d', eta_bins)
# 
# phi_bins = -
# phi_bins = array('d', [])
# 
# h_eta_phi = ROOT.TH1I('h_eta_phi', '', 40, eta_bins, 120, phi_bins)
# for softid in range(1, 4801):
#     h_eta_phi.Fill(mapping.bemc(softid).eta, mapping.bemc(softid).phi, softid)

def is_electron(track):
    eta_cut = abs( track.eta() ) < 1.0
    dca_cut = abs( track.globalDca().mag() ) < 1.0
    fit_cut = track.nHitsFit() > 25
    if eta_cut and dca_cut and fit_cut:
        pt = track.Pt()
        nsigmapi = track.nSigmaPion()
        if pt > 8.80:
            return nsigmapi > 2.10
        elif pt > 3.18:
            return nsigmapi > 2.40
        else:
            return nsigmapi > 2.60
    return False


def project_track(swimmer, track):
    """
    This method is no good.  It uses the inner helix instead of the outer one,
    and it doesn't even work then (problems casting StPhysicalHelixD)
    """
    helix = track.globalHelix()
    radius = 225.405
    result = helix.pathLength(radius)
    s = (result.first > 0) and result.first or result.second
    at_final = helix.at(s)
    return at_final.pseudoRapidity(), at_final.phi()
    # pos = ROOT.StThreeVectorD()
    # mom = ROOT.StThreeVectorD()
    # swimmer.projTrack(pos, mom, track.globalHelix(), track.B())
    # return mom.Eta(),mom.Phi()


def match_tower(swimmer, eta, phi):
    return swimmer.getNextTowerId(eta, phi, 0, 0)


def find_tower_particle(event, towerid):
    for jet in event.jets():
        for particle in jet.particles():
            if particle.detectorId() == ROOT.kBarrelEmcTowerId:
                if particle.index() == towerid:
                    return particle
    return None


def make_ntuple(tree):
    swimmer = ROOT.StEmcPosition()
    geometry = ROOT.StEmcGeom(1)
    tree.GetEntry(0)
    run = tree.event.runId()
    f = ROOT.TFile('2006_electrons_%d.root' % (run,), 'recreate')
    nt = ROOT.TNtuple('nt', '', 'event:ht:tp:vz:p:energy:id:eta:phi:deta:dphi')
    vals = [0.0 for i in range(11)]
    for entry in tree:
        ev = tree.event
        for track in filter(is_electron, ev.tracks()):
            eta, phi = project_track(swimmer, track)
            tower_id = match_tower(swimmer, eta, phi)
            print eta,phi,tower_id
            tower_eta = mapping.bemc(tower_id).eta
            tower_phi = mapping.bemc(tower_id).phi
            print eta,phi,tower_id, tower_eta, tower_phi
            trigger_patch = mapping.bemc(tower_id).triggerPatch
            tower = find_tower_particle(ev, tower_id)
            if tower:
                vals[0] = ev.eventId()
                vals[1] = ev.highTowerAdc(tower.index()) > 0
                vals[2] = ev.triggerPatchAdc(trigger_patch) > 0
                vals[3] = ev.vertex(0).z()
                vals[4] = track.P()
                vals[5] = tower.E()
                vals[6] = tower_id
                vals[7] = tower_eta
                vals[8] = tower_phi
                vals[9] = tower_eta - eta
                vals[10]= tower_phi - phi
                nt.Fill(*vals)
    
    f.cd()
    nt.Write()
    f.Close()

