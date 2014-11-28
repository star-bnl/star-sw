import math
import os
import sys
import time
import sets
from array import array

import ROOT
import minimc
import analysis

import mcasym

simu = False
year = 2006

ptBins      = minimc.MiniMcHistos.ptBins
etaBins     = minimc.MiniMcHistos.etaBins
phiBins     = minimc.MiniMcHistos.phiBins
nFitBins    = minimc.MiniMcHistos.nFitBins
pBins       = minimc.MiniMcHistos.pBins
dEdxBins    = minimc.MiniMcHistos.dEdxBins
dcaGBins    = minimc.MiniMcHistos.dcaGBins
vzBins      = minimc.MiniMcHistos.vzBins

zbins       = [0.0, 0.075, 0.125, 0.2, 0.3, 0.45, 0.65, 1.0]
zar = array('d', zbins)

asymKeys = ['STD', 'MAX', 'MIN', 'ZERO', 'GS_NLOC', 'DSSV', 'LSS1', 'LSS2',
    'LSS3', 'AAC1', 'DNS1', 'DNS2', 'M015', 'M030', 'M045', 'denom']

pidCalibration = {
6988 : ( 0.066608, 0.889596),
6990 : ( 0.028513, 0.872612),
6992 : ( 0.016085, 0.856032),
6994 : ( 0.076727, 0.867838),
6995 : ( 0.167452, 0.855377),
6997 : ( 0.206903, 0.882462),
6998 : (-0.018938, 0.844754),
7001 : (-0.006526, 0.863949),
7002 : ( 0.014921, 0.865907),
7032 : (-0.059741, 0.856267),
7034 : (-0.064317, 0.870471),
7035 : (-0.090822, 0.871124),
7048 : (-0.387454, 0.866186),
7049 : (-0.393791, 0.865776),
7051 : (-0.137894, 0.792171),
7055 : (-0.420703, 0.858151),
7064 : (-0.283141, 0.878024),
7067 : (-0.087372, 0.873845),
7068 : (-0.077816, 0.840146),
7069 : ( 0.010756, 0.861486),
7070 : (-0.054644, 0.879342),
7072 : (-0.022931, 0.863193),
7075 : (-0.065000, 0.814292),
7079 : (-0.261988, 0.896589),
7085 : (-0.207958, 0.879845),
7087 : (-0.062486, 0.818310),
7088 : (-0.184964, 0.869739),
7092 : (-0.095016, 0.879631),
7102 : (-0.161471, 0.899666),
7103 : (-0.180760, 0.900305),
7110 : (-0.194342, 0.894236),
7112 : (-0.116899, 0.878893),
7114 : (-0.140619, 0.871448),
7118 : (-0.206615, 0.883038),
7120 : (-0.096027, 0.884061),
7122 : (-0.187495, 0.903031),
7123 : (-0.172261, 0.885190),
7124 : (-0.175057, 0.906155),
7125 : (-0.134531, 0.887071),
#7127 : (-0.149092, 0.897838), ## RunLog_onl problem
7128 : (-0.149092, 0.897838),
7131 : (-0.197370, 0.889249),
7133 : (-0.207133, 0.888365),
#7134 : (-0.130486, 0.893232), ## RunLog_onl problem
7134 : (-0.119566, 0.901472),
7136 : (-0.026902, 0.621362),
7138 : (-0.150951, 0.892991),
7151 : ( 0.033011, 0.851425),
7153 : (-0.094305, 0.892003),
7154 : ( 0.010605, 0.824362),
7161 : (-0.167815, 0.894401),
7162 : (-0.152793, 0.906894),
7164 : (-0.125891, 0.894419),
7165 : (-0.162527, 0.870979),
7166 : (-0.133315, 0.884772),
7172 : (-0.207923, 0.894637),
7232 : (-0.110842, 0.864740),
7237 : (-0.269559, 0.916554),
7238 : (-0.259372, 0.891293),
7249 : (-0.247810, 0.894708),
7250 : (-0.316081, 0.909442),
7253 : (-0.271044, 0.845922),
7255 : (-0.249242, 0.857201),
7265 : (-0.190056, 0.865755),
7266 : (-0.243196, 0.869884),
7269 : (-0.242802, 0.892759),
7270 : (-0.314700, 0.886310),
7271 : (-0.389984, 0.910892),
7272 : (-0.194932, 0.883244),
7274 : (-0.344731, 0.903323),
7276 : (-0.198418, 0.897573),
7278 : (-0.273830, 0.875383),
7279 : (-0.219441, 0.895625),
7293 : (-0.346773, 0.803589), ## transverse
7295 : (-0.268542, 0.859643), ## transverse
7296 : (-0.328538, 0.901968), ## transverse
7300 : (-0.283700, 0.876021),
7301 : (-0.325708, 0.891317),
7302 : (-0.333164, 0.890520),
7303 : (-0.274641, 0.894173),
7304 : (-0.306279, 0.890682),
7305 : ( 0.726845, 0.934432),
7308 : (-0.297895, 0.913868),
7311 : (-0.232436, 0.905442),
7317 : (-0.301910, 0.886113),
7320 : (-0.229533, 0.893824),
7325 : (-0.251390, 0.898416),
7327 : (-0.251943, 0.895118),
7718 : (-0.077766, 0.879930),  ## transverse
7722 : (-0.019419, 0.848818),  ## transverse
7724 : (-0.115915, 0.875705),  ## transverse
7725 : (-0.089028, 0.869330),  ## transverse
7729 : (-0.149134, 0.896646),  ## transverse
7739 : (-0.092455, 0.885406),  ## transverse
7740 : (-0.108566, 0.891201),  ## transverse
7744 : (-0.051341, 0.894509),  ## transverse
7745 : (-0.072091, 0.864465),  ## transverse
7753 : (-0.051187, 0.889440),  ## transverse
7756 : ( 0.151309, 0.848012),  ## transverse
7757 : (-0.108126, 0.880723),  ## transverse
7781 : (-0.045714, 0.907452),  ## transverse
7785 : ( 0.079990, 0.866071),  ## transverse
7786 : (-0.043541, 0.882520),  ## transverse
7788 : ( 0.063298, 0.888150),  ## transverse
7789 : (-0.115123, 0.894880),  ## transverse
7790 : (-0.163721, 0.903411),  ## transverse
7791 : (-0.107792, 0.909530),  ## transverse
7792 : (-0.058317, 0.892502),  ## transverse
7794 : (-0.062679, 0.903440),  ## transverse
7795 : (-0.120403, 0.904253),  ## transverse
7796 : (-0.208192, 0.927860),  ## transverse
7797 : (-0.157988, 0.910834),  ## transverse
7800 : (-0.135945, 0.889122),  ## transverse
7803 : (-0.024001, 0.873819),  ## transverse
7804 : (-0.253835, 0.897332),  ## transverse
7805 : (-0.176320, 0.895462),  ## transverse
7810 : (-0.084868, 0.877880),  ## transverse
7811 : (-0.157192, 0.863181),  ## transverse
7815 : (-0.270277, 0.898946),  ## transverse
7817 : (-0.196213, 0.880809),  ## transverse
7820 : (-0.238482, 0.899890),  ## transverse
7823 : (-0.247331, 0.894104),  ## transverse
7824 : (-0.278816, 0.909190),  ## transverse
7825 : (-0.174100, 0.886618),  ## transverse
7826 : (-0.234116, 0.887850),  ## transverse
7827 : (-0.171585, 0.863359),  ## transverse
7830 : (-0.238725, 0.895052),  ## transverse
7831 : (-0.222429, 0.897187),  ## transverse
7847 : (-0.027059, 0.874814),
7850 : (-0.123179, 0.900492),
7851 : (-0.036289, 0.889293),
7852 : (-0.045614, 0.891709),
7853 : (-0.119185, 0.898291),
7855 : (-0.134738, 0.908692),
7856 : (-0.089265, 0.895362),
7858 : (-0.102251, 0.897319),
7863 : (-0.083709, 0.911717),
7864 : (-0.113995, 0.904617),
7865 : ( 0.009516, 0.813364),
7871 : (-0.109983, 0.906486),
7872 : (-0.138412, 0.882469),
7883 : (-0.182051, 0.885964),
7886 : (-0.185474, 0.889406),
7887 : (-0.155607, 0.886540),
7889 : (-0.100630, 0.863594),
7890 : (-0.053211, 0.921421),
7891 : (-0.099581, 0.861152),
7892 : (-0.077996, 0.892110),
7893 : (-0.123162, 0.907638),
7896 : (-0.094806, 0.894019),
7898 : (-0.057477, 0.896248),
7901 : (-0.014140, 0.865671),
7908 : (-0.040456, 0.883598),
7909 : (-0.077470, 0.880354),
7911 : (-0.048150, 0.861430),
7913 : (-0.076636, 0.896822),
7916 : (-0.078441, 0.894539),
7918 : ( 0.043434, 0.888361),
7921 : (-0.010669, 0.876733),
7922 : ( 0.022409, 0.907525),
7926 : (-0.033154, 0.886284),
7944 : (-0.068744, 0.905098),
7949 : (-0.039570, 0.876990),
7951 : ( 0.056927, 0.936951),
7952 : ( 0.025446, 0.885525),
7954 : ( 0.205871, 0.864685),
7957 : ( 0.014065, 0.899964)
}

class EventCuts:
    def __init__(self, event=None):
        if event is None:
            self.vertex = False
            self.bbc         = False
            self.all         = False
        else:
            self.set(event)
    
    
    def set(self, event):
        self.vertex = event.nVertices() > 0
        
        timeDiff = event.bbcTimeBin()
        #if timeDiff % 32 != 0:
        #    self.bbc = False
        #else:
        #    bin = timeDiff / 32
        #    self.bbc = bin in (7,8,9) or (event.runId() > 7000000 and bin == 6)
        
        ## dropping the discrete timebin cut -- APK 2008-01-09
        bin = timeDiff / 32
        self.bbc = bin in (7,8,9) or (event.runId() > 7000000 and bin == 6)
        
        if simu: self.bbc = True
        
        self.all = self.vertex and self.bbc
    


class TrackCuts:
    def __init__(self, fill):
        self.eta = False
        self.dca = False
        self.fit = False
        self.pid = False
        self.all = False
        
        ## need this try..except to bootstrap PID calibration
        try:
            pidFit = pidCalibration[fill]
        except KeyError:
            pidFit = (0.0, 1.0)
        #pidFit = pidCalibration[fill]
        self.pidMin = pidFit[0] - 1.0*pidFit[1]
        self.pidMax = pidFit[0] + 2.0*pidFit[1]
        
        ## try a "cleaner" bg sample at least 2 sigma from pion mean
        self.pid_bg = False
        self.pidBgMin = pidFit[0] - 2.0*pidFit[1]
        self.pidBgMax = self.pidMax
    
    
    def set(self, track):
        self.eta = math.fabs( track.eta() ) < 1.0
        self.dca = math.fabs( track.globalDca().mag() ) < 1.0
        self.fit = track.nHitsFit() > 25
        
        self.pid = self.pidMin < track.nSigmaPion() < self.pidMax
        self.pid_bg = (track.nSigmaPion() < self.pidBgMin) or \
                      (track.nSigmaPion() > self.pidBgMax)
        
        if simu: 
            self.pid = True
            self.pid_bg = False
        self.all = self.eta and self.dca and self.pid and self.fit
        


class JetCuts:
    triggerThresholds = { 
        96201:13, 
        96211:17, 
        96221:66, 
        96233:83, 
        137221:58, 
        137222:60 
    }
    minPhi2005 = 40
    maxPhi2005 = 320
    patchPhi2005 = [90.,30.,-30.,-90.,-150.,150.]
    minPhi2006 = 36
    maxPhi2006 = 324
    patchPhi2006 = [150.,90.,30.,-30.,-90.,-150.,150.,90.,30.,-30.,-90.,-150.]
    
    
    def __init__(self, jet=None, event=None):
        if jet is None:
            self.eta = False
            self.rt = False
            self.trig = []
        else:
            self.set(jet, event)
    
    
    def set(self, jet, event):
        if (not simu and event.runId() < 7000000) or (simu and year == 2005):
            self.eta = 0.2 < jet.detectorEta() < 0.8
            self.rt = 0.1 < (jet.tpcEtSum() / jet.Et()) < 0.9
            self.trig = [96011]
            
            ## HT geometric trigger condition
            for particle in jet.particles():
                if particle.detectorId() == ROOT.kBarrelEmcTowerId:
                    adc = event.highTowerAdc(particle.index())
                    if adc > self.triggerThresholds[96211]:
                        self.trig.append(96201)
                        self.trig.append(96211)
                    elif adc > self.triggerThresholds[96201]:
                        self.trig.append(96201)
            
            ## JP geometric trigger condition
            for patchId in range(6):
                adc = event.jetPatchAdc(patchId)
                if adc > self.triggerThresholds[96221]:
                    dPhi=abs(math.degrees(jet.Phi())-self.patchPhi2005[patchId])
                    if dPhi < self.minPhi2005 or dPhi > self.maxPhi2005:
                        self.trig.append(96221)
                        if adc > self.triggerThresholds[96233]:
                            self.trig.append(96233)
        else:
            self.eta = -0.7 < jet.detectorEta() < 0.9
            self.rt = (jet.tpcEtSum() / jet.Et()) > 0.08
            self.trig = [117001]
        
            ## JP geometric trigger condition
            for patchId in range(12):
                adc = event.jetPatchAdc(patchId)
                if adc > self.triggerThresholds[137221]:
                    dPhi=abs(math.degrees(jet.Phi())-self.patchPhi2006[patchId])
                    if dPhi < self.minPhi2006 or dPhi > self.maxPhi2006:
                        self.trig.append(137221)
                        if adc > self.triggerThresholds[137222]:
                            self.trig.append(137222)
        
    


class Histo(object):
    """
    wrapper around a ROOT histogram so I can add a few convenience methods
    """
    def __init__(self, hist):
        self.h = hist
        self.profile = (type(hist) == ROOT.TProfile)
        if not self.profile:
            self.h.Sumw2()
        self.vals = []
    
    def __getattr__(self, name):
        """
        fall back to ROOT method if I didn't define a replacement
        """
        return getattr(self.h, name)
    
    def Fill(self, x, y=None, z=None):
        if self.profile:
            self.h.Fill(x,y)
        else:
            self.vals.append((x,y,z))
    
    def Flush(self):
        """
        Ends the event and actually fills the ROOT histogram, taking multi-
        particle statistics into account.
        """
        if self.profile: return
        weight = {}
        keep = {}
        for x,y,z in self.vals:
            bin = self.h.FindBin(x, y or 0, z or 0)
            weight[bin] = weight.get(bin,0) + 1
            keep[bin] = (x,y,z)
        for bin,w in weight.items():
            x,y,z = keep[bin]
            if z:
                self.h.Fill(x,y,z,w)
            elif y:
                try:
                    self.h.Fill(x,y,w)
                except TypeError:
                    # assume this is an mcasym histo which has its own weight
                    self.h.Fill(x,y)
            else:
                self.h.Fill(x,w)
        self.vals = []
    


class TrackHistogramCollection(dict):
    """histograms filled for each track"""
    allKeys = [
        'pt', 'eta', 'phi', 'nHitsFit', 'dEdx', 'dcaG', 'nSigmaPion',
        'dphi_deta', 'z', 'z_away', 'pt_near', 'pt_away', 'pt_bg', 'z_jet',
        'z_away_jet', 'xi', 'xi_away', 'ptMc_ptPr', 'ptMc_ptGl', 'etaMc_etaPr',
        'etaMc_etaGl', 'ptMc', 'away_mult', 'near_mult', 'away_lead_pt', 
        'near_lead_pt', 'lead_matched', 'lead_cutfail', 'lead_nomatch', 
        'z_away2', 'z_away3', 'z_away4', 'away2_eta', 'away2_nHitsFit', 
        'away2_dcaG', 'away2_nSigmaPion', 'z_away2_bg', 'vz', 'distortedPt', 
        'dphi', 'one', 'meanpt', 'meanjetpt', 'z_noshift', 'zf', 'z_away2_low',
        'z_away2_high'
    ]
    for key in asymKeys:
        allKeys.extend([key, key+'w', key+'f', key+'l', key+'h'])
    
    def __init__(self, name, tfile=None, keys=None):
        self.away_mult = 0
        self.near_mult = 0
        self.away_lead_pt = 0.0
        self.near_lead_pt = 0.0
        if tfile is not None:
            for key in self.allKeys:
                if keys is None or key in keys:
                    self[key] = tfile.Get('%s_%s' % (name,key))
        else:
            if simu:
                self['ptMc_ptPr'] = Histo(ROOT.TH2D('%s_ptMc_ptPr' % name, \
                    'prim reco p_{T} vs. true p_{T}', \
                    5*ptBins[0], ptBins[1], ptBins[2], \
                    5*ptBins[0], ptBins[1], ptBins[2]))
                self['ptMc_ptGl'] = Histo(ROOT.TH2D('%s_ptMc_ptGl' % name, \
                    'glob reco p_{T} vs. true p_{T}', \
                    ptBins[0], ptBins[1], ptBins[2], \
                    ptBins[0], ptBins[1], ptBins[2]))
                self['etaMc_etaPr'] = Histo(ROOT.TH2D('%s_etaMc_etaPr' % name, \
                    'prim reco #eta vs. true #eta', \
                    etaBins[0], etaBins[1], etaBins[2], \
                    etaBins[0], etaBins[1], etaBins[2]))
                self['etaMc_etaGl'] = Histo(ROOT.TH2D('%s_etaMc_etaGl' % name, \
                    'glob reco #eta vs. true #eta', \
                    etaBins[0], etaBins[1], etaBins[2], \
                    etaBins[0], etaBins[1], etaBins[2]))
                self['ptMc'] = Histo(ROOT.TH1D('%s_ptMc' % name, \
                    'true track p_{T}', \
                    ptBins[0], ptBins[1], ptBins[2]))
                
                if year == 2005:
                    self['STD'] = Histo(ROOT.TH1D('%s_STD' % name, \
                        'GRSV-STD', ptBins[0], ptBins[1], ptBins[2]))
                    self['MAX'] = Histo(ROOT.TH1D('%s_MAX' % name, \
                        'GRSV-MAX', ptBins[0], ptBins[1], ptBins[2]))
                    self['MIN'] = Histo(ROOT.TH1D('%s_MIN' % name, \
                        'GRSV-MIN', ptBins[0], ptBins[1], ptBins[2]))
                    self['ZERO'] = Histo(ROOT.TH1D('%s_ZERO' % name, \
                        'GRSV-ZERO', ptBins[0], ptBins[1], ptBins[2]))
                    self['GS_NLOC'] = Histo(ROOT.TH1D('%s_GS_NLOC' % name, \
                        'GS Set C', ptBins[0], ptBins[1], ptBins[2]))
                    self['denom'] = Histo(ROOT.TH1D('%s_denom' % name, \
                        'Asymmetry denominator', \
                        ptBins[0], ptBins[1], ptBins[2]))
                else:
                    for key in ('STD', 'MAX', 'MIN', 'ZERO', 'GS_NLOC', 'DSSV',
                        'LSS1', 'LSS2', 'LSS3', 'AAC1', 'DNS1', 'DNS2', 'M015',
                        'M030', 'M045'):
                        self[key] = Histo(ROOT.TH1D('%s_%s' % (name,key), 
                            key, len(zbins)-1, zar))
                        self[key+'f'] = Histo(ROOT.TH1D('%s_%sf' % (name,key), 
                            key, 20, 0., 1.))
                        self[key+'w'] = Histo(ROOT.TH1D('%s_%sw' % (name,key), 
                            key, len(zbins)-1, zar))
                        self[key+'l'] = Histo(ROOT.TH1D('%s_%sl' % (name,key),
                            key, len(zbins)-1, zar))
                        self[key+'h'] = Histo(ROOT.TH1D('%s_%sh' % (name,key),
                            key, len(zbins)-1, zar))
                    
                    self['denom'] = Histo(ROOT.TH1D('%s_denom' % name, \
                        'Asymmetry denominator', len(zbins)-1, zar))
                    self['denomf'] = Histo(ROOT.TH1D('%s_denomf' % name, \
                        'Asymmetry denominator', 20, 0., 1.))
                    self['denomw'] = Histo(ROOT.TH1D('%s_denomw' % name, \
                        'Asymmetry denominator', len(zbins)-1, zar))
                    self['denoml'] = Histo(ROOT.TH1D('%s_denoml' % name, \
                        'Asymmetry denominator', len(zbins)-1, zar))
                    self['denomh'] = Histo(ROOT.TH1D('%s_denomh' % name, \
                        'Asymmetry denominator', len(zbins)-1, zar))
            
            self['one'] = Histo(ROOT.TH1D('%s_one' % name, '', 1, -0.5, 0.5))
            
            self['pt'] = Histo(ROOT.TH1D('%s_pt' % (name,), 'track p_{T}', \
                ptBins[0], ptBins[1], ptBins[2]))
            self['eta'] = Histo(ROOT.TH1D('%s_eta' % (name,), 'track #eta', \
                etaBins[0], etaBins[1], etaBins[2]))
            self['phi'] = Histo(ROOT.TH1D('%s_phi' % (name,), 'track #phi', \
                phiBins[0], phiBins[1], phiBins[2]))
            self['nHitsFit'] = Histo(ROOT.TH1D('%s_nHitsFit' % (name,), \
                'nHitsFit', nFitBins[0], nFitBins[1], nFitBins[2]))
            self['dEdx'] = Histo(ROOT.TH1D('%s_dEdx' % (name,), 'dE/dx', \
                dEdxBins[0], dEdxBins[1], dEdxBins[2]))
            self['dcaG'] = Histo(ROOT.TH1D('%s_dcaG' % (name,), '|dcaGlobal|', \
                dcaGBins[0], dcaGBins[1], dcaGBins[2]))
            self['vz'] = Histo(ROOT.TH1D('%s_vz' % (name,), \
                'vz for good tracks', vzBins[0], vzBins[1], vzBins[2]))
            
            self['nSigmaPion'] = Histo(ROOT.TH1D('%s_nSigmaPion' % (name,), \
                'n#sigma(#pi)', 240, -6.0, 6.0))
            
            self['dphi_deta'] = Histo(ROOT.TH2D('%s_dphi_deta' % (name,), \
                'track relative to trigger jet', \
                50, -1.5*math.pi, 0.5*math.pi, 50, -2.0, 2.0))
            self['dphi_deta'].SetXTitle('#delta#phi')
            self['dphi_deta'].SetYTitle('#delta#eta')
            z_xbins = [2.0, 3.0, 4.0, 5.5, 7.0, 10.0]
            ar = array('d',z_xbins)
            
            self['z'] = Histo(ROOT.TH2D('%s_z' % (name,), '<z>', \
                len(z_xbins)-1, ar, 50, 0., 1.))
            self['z_away'] = Histo(ROOT.TH2D('%s_z_away' % (name,), \
                '<z> for away-side jet', len(z_xbins)-1, ar, 50, 0., 1.))
            
            self['pt_away'] = Histo(ROOT.TH1D('%s_pt_away' % (name,), \
                'track p_{T} away-side', ptBins[0], ptBins[1], ptBins[2]))
            self['pt_near'] = Histo(ROOT.TH1D('%s_pt_near' % (name,), \
                'track p_{T} near-side', ptBins[0], ptBins[1], ptBins[2]))
            
            self['pt_bg'] = Histo(ROOT.TH1D('%s_pt_bg' % name, \
                'track p_{T} for PID bg', ptBins[0], ptBins[1], ptBins[2]))
            
            # www.star.bnl.gov/protected/spin/rfatemi/Jet_2005/Binning/BIN.html
            jet_et_bins = [5.0, 6.15, 7.5645, 9.30434, 11.4443, 14.0765, 
                17.3141, 21.2964, 26.1945, 32.2193, 39.6297, 48.7446, 59.9558]
            ar = array('d', jet_et_bins)
            self['z_jet'] = Histo(ROOT.TH2D('%s_z_jet' % name, \
                'z vs. jet E_{T}', len(jet_et_bins)-1, ar, 50, 0., 1.))
            self['z_away_jet'] = Histo(ROOT.TH2D('%s_z_away_jet' % name, \
                'z vs. away jet E_{T}', len(jet_et_bins)-1, ar, 50, 0., 1.))
            self['xi'] = Histo(ROOT.TH2D('%s_xi' % name, '#Xi', \
                len(jet_et_bins)-1, ar, 50, 0., 4.))
            self['xi_away'] = Histo(ROOT.TH2D('%s_xi_away' % name, \
                '#Xi for away-side tracks', len(jet_et_bins)-1, ar, 50, 0., 4.))
                
            self['away_mult'] = Histo(ROOT.TH1D('%s_away_mult' % name, \
                '# of pions on away-side', 10, -0.5, 9.5))
            self['near_mult'] = Histo(ROOT.TH1D('%s_near_mult' % name, \
                '# of pions on near-side', 10, -0.5, 9.5))
            
            self['away_lead_pt'] = Histo(ROOT.TH1D('%s_away_lead_pt' % name, \
                'p_{T} of leading pion on away-side', \
                ptBins[0], ptBins[1], ptBins[2]))
            self['near_lead_pt'] = Histo(ROOT.TH1D('%s_near_lead_pt' % name, \
                'p_{T} of leading pion on near-side', \
                ptBins[0], ptBins[1], ptBins[2]))
            
            self['lead_matched'] = Histo(ROOT.TH1D('%s_lead_matched' % name, \
                'p_{T} of leading pion on away-side', \
                ptBins[0], ptBins[1], ptBins[2]))
            self['lead_cutfail'] = Histo(ROOT.TH1D('%s_lead_cutfail' % name, \
                'p_{T} of leading pion on away-side', \
                ptBins[0], ptBins[1], ptBins[2]))
            self['lead_nomatch'] = Histo(ROOT.TH1D('%s_lead_nomatch' % name, \
                'p_{T} of leading pion on away-side', \
                ptBins[0], ptBins[1], ptBins[2]))
            
            self['z_noshift'] = Histo(ROOT.TH1D('%s_z_noshift' % name, '', \
                len(zbins)-1, zar))            
            self['z_away2'] = Histo(ROOT.TH1D('%s_z_away2' % name, '', \
                len(zbins)-1, zar))
            self['z_away2'].SetXTitle('p_{T}(#pi)/p_{T}(trigger jet)')
            
            self['z_away2_low'] = Histo(ROOT.TH1D('%s_z_away2' % name, '', \
                len(zbins)-1, zar))
            self['z_away2_high'] = Histo(ROOT.TH1D('%s_z_away2' % name, '', \
                len(zbins)-1, zar))
                
            self['meanpt'] = Histo(ROOT.TProfile('%s_meanpt' % name, '', \
                len(zbins)-1, zar))
            self['meanjetpt'] = Histo(ROOT.TProfile('%s_meanjetpt' % name, '', \
                len(zbins)-1, zar))
            
            self['z_away3'] = Histo(ROOT.TH1D('%s_z_away3' % name, '', \
                len(zbins)-1, zar))
            self['z_away3'].SetXTitle(\
                'fraction of non-trigger jet p carried by #pi')
            
            self['z_away4'] = Histo(ROOT.TH1D('%s_z_away4' % name, \
                'inclusive if separate z-bins', len(zbins)-1, zar))
            self['z_away4'].SetXTitle('p_{T}(#pi)/p_{T}(trigger jet)')
            
            self['away2_eta'] = Histo(ROOT.TH1D('%s_away2_eta' % (name,), \
                'away-side #eta', etaBins[0], etaBins[1], etaBins[2]))
            self['away2_dcaG'] = Histo(ROOT.TH1D('%s_away2_dcaG' % (name,), \
                'away-side |dcaG|', dcaGBins[0], dcaGBins[1], dcaGBins[2]))
            self['away2_nHitsFit'] = Histo(ROOT.TH1D('%s_away2_nHitsFit'% name,\
                'away-side nHitsFit', nFitBins[0], nFitBins[1], nFitBins[2]))
            self['away2_nSigmaPion'] = Histo(ROOT.TH1D('%s_away2_nSigmaPion' \
                % (name,), 'away-side n#sigma(#pi)', 240, -6.0, 6.0))
            
            self['z_away2_bg'] = Histo(ROOT.TH1D('%s_z_away2_bg' % name, '', \
                len(zbins)-1, zar))
            self['z_away2_bg'].SetXTitle('p_{T}(#pi)/p_{T}(trigger jet)')
            
            self['distortedPt'] = Histo(ROOT.TH1D('%s_distortedPt' % name, \
                'pT corrected for space charge distortion', \
                ptBins[0], ptBins[1], ptBins[2]))
            
            self['dphi'] = Histo(ROOT.TH1D('%s_dphi' % name, \
                '#Delta#phi relative to trigger jet', \
                2*phiBins[0], phiBins[1], phiBins[2]))
            
            self['zf'] = Histo(ROOT.TH1D('%s_zf' % name,
                'finely binned for mcasym comparison', 20, 0., 1.))
    
    
    def fillMcTrack(self, track):
        """docstring for fillMcTrack"""
        pass
    
    
    def fillMatchedPair(self, track):
        """docstring for fillMatchedPair"""
        if track.geantId() in (8,9):
            self['ptMc'].Fill(track.ptMc())
            self['ptMc_ptPr'].Fill(track.ptMc(), track.ptPr())
            self['ptMc_ptGl'].Fill(track.ptMc(), track.ptGl())
            self['etaMc_etaPr'].Fill(track.etaMc(), track.etaPr())
            self['etaMc_etaGl'].Fill(track.etaMc(), track.etaGl())
    
    
    def fillTrack(self, track, tcuts):
        if tcuts.eta and tcuts.dca and tcuts.fit and tcuts.pid:
            self['pt'].Fill(track.pt())
            self['phi'].Fill(track.phi())
            self['dEdx'].Fill(track.dEdx() * 1e7)
            self['distortedPt'].Fill(distortedPt(track))
        
        if tcuts.eta and tcuts.dca and tcuts.fit and tcuts.pid_bg:
            self['pt_bg'].Fill(track.pt())
        
        if tcuts.dca and tcuts.fit and tcuts.pid:
            self['eta'].Fill(track.eta())
        
        if tcuts.eta and tcuts.fit and tcuts.pid:
            self['dcaG'].Fill(track.globalDca().mag())
        
        if tcuts.eta and tcuts.dca and tcuts.pid:
            self['nHitsFit'].Fill(track.nHitsFit())
            
        if tcuts.eta and tcuts.dca and tcuts.fit:
            self['nSigmaPion'].Fill(track.nSigmaPion())
    
    
    def fillTrackJetPair(self, track, tcuts, jet, awayjet=None):
        """all jet-pion correlation studies go here"""
        if tcuts.eta and tcuts.dca and tcuts.fit and tcuts.pid:
            deta = track.Eta() - jet.Eta()
            dphi = track.Phi() - jet.Phi()
            
            #normalize dphi
            if dphi < -1.5*math.pi:
                dphi = dphi + 2*math.pi
            if dphi > 0.5*math.pi:
                dphi = dphi - 2*math.pi
                  
            self['dphi_deta'].Fill(dphi, deta)
                        
            dR = track.DeltaR(jet)
            if dR < 0.4:
                self.near_mult += 1
                self.near_lead_pt = max(self.near_lead_pt, track.Pt())
                z = track.Vect().Dot(jet.Vect()) / jet.P()**2
                xi = math.log(jet.E() / track.P())
                self['z'].Fill(track.Pt(), z)
                self['z_jet'].Fill(jet.Pt(), z)
                self['xi'].Fill(jet.Pt(), xi)
                self['pt_near'].Fill(track.Pt())
            elif dR > 1.5:
                self.away_mult += 1
                self.away_lead_pt = max(self.away_lead_pt, track.Pt())
                if awayjet is not None:
                    z_away = track.Vect().Dot(awayjet.Vect()) / awayjet.P()**2
                    xi = math.log(awayjet.E() / track.P())
                    self['z_away'].Fill(track.Pt(), z_away)
                    self['z_away_jet'].Fill(awayjet.Pt(), z_away)
                    self['xi_away'].Fill(awayjet.Pt(), xi)
                self['pt_away'].Fill(track.Pt())
                
    
    
    def histos(self):
        val = self.values()
        return [v for v in val if v is not None]
    
    
    def Clear(self):
        self.away_mult = 0
        self.near_mult = 0
        self.away_lead_pt = 0.0
        self.near_lead_pt = 0.0
    
    
    def Add(self, other):
        [ h.Add(other[key].h) for key, h in self.items() ]
    
    
    def Flush(self):
        [ h.Flush() for h in self.values() ]
    
    
    def Write(self):
        """make it persistent"""
        [ h.Write() for h in self.values() ]
    
    


class HistogramCollection(dict):
    """
    convenience class for dealing with multiple histos all filled the same way
    """
    mcVzBins         = minimc.MiniMcHistos.vzBins
    mcPtBins         = minimc.MiniMcHistos.ptBins
    
    allKeys = ['nVertices', 'vx_vy', 'vz', 'vzBBC', 'spinBit', 'bx7', 'bbc', 
        'jet_pt_balance', 'jet_p_balance', 'jet_eta_balance', 'jet_phi_balance',
        'cosTheta', 'hardP', 'x1','x2','x1_x2', 
        'jet_pt', 'lead_neutral', 'inclusive_jet_mult', 'dijet_mult',
        'true_jet_pt', 'jet_ptw']
    
    def __init__(self, name, tfile=None, keys=None):
        super(HistogramCollection, self).__init__()
        if tfile is not None:
            for key in self.allKeys:
                if keys is None or key in keys:
                    self[key] = tfile.Get('%s_%s' % (name,key))
        else:
            if simu:
                self['cosTheta'] = Histo(ROOT.TH1D('%s_cosTheta' % name, \
                    'cos(#Theta)', 200, -1.0, 1.0))
                self['hardP'] = Histo(ROOT.TH1D('%s_hardP' % name, \
                    'partonic p_{T}', 280, 0.0, 70.0))
                self['x1'] = Histo(ROOT.TH1D('%s_x1' % name, 'x1', 200, 0., 1.))
                self['x2'] = Histo(ROOT.TH1D('%s_x2' % name, 'x2', 200, 0., 1.))
                self['x1_x2'] = Histo(ROOT.TH2D('%s_x1_x2' % name, \
                    'x1 vs. x2', 200, 0., 1., 200, 0., 1.))
            
            self['nVertices'] = Histo(ROOT.TH1D('%s_nVertices' % (name,),'', \
                15,-0.5,14.5))
            self['vx_vy'] = Histo(ROOT.TH2D('%s_vx_vy' % (name,),'', \
                400,-2.0,2.0, 400,-2.0,2.0))
            self['vz'] = Histo(ROOT.TH1D('%s_vz' % (name,),'', \
                vzBins[0], vzBins[1], vzBins[2]))
            self['vzBBC'] = Histo(ROOT.TH1D('%s_vzBBC' % (name,),'', \
                vzBins[0], vzBins[1], vzBins[2]))
            self['spinBit'] = Histo(ROOT.TH1D('%s_spinBit' % (name,),'', \
                17,0.5,16.5))
            self['bx7'] = Histo(ROOT.TH1D('%s_bx7' % (name,),'',128,-0.5,127.5))
            self['bbc'] = Histo(ROOT.TH1D('%s_bbc' % (name,),'',400,-0.5,399.5))
            self['jet_pt_balance'] = Histo(ROOT.TH2D('%s_jet_pt_balance' \
                % name,'',200,0.0,50.0, 200,0.0,50.0))
            self['jet_p_balance'] = Histo(ROOT.TH2D('%s_jet_p_balance' \
                % name,'',200,0.0,50.0, 200,0.0,50.0))
            self['jet_eta_balance'] = Histo(ROOT.TH2D('%s_jet_eta_balance' \
                % name,'',100,-2.0,2.0, 100,-2.0,2.0))
            self['jet_phi_balance'] = Histo(ROOT.TH2D('%s_jet_phi_balance' \
                % name,'',100,-math.pi,math.pi, 100,-math.pi,math.pi))
            self['jet_pt'] = Histo(ROOT.TH1D('%s_jet_pt' % name, '', \
                200, 0.0, 50.0))
            self['lead_neutral'] = Histo(ROOT.TH1D('%s_lead_neutral' % name, \
                'pT spectrum for neutral leading particles', \
                ptBins[0], ptBins[1], ptBins[2]))
            self['inclusive_jet_mult'] = Histo(ROOT.TH1D('%s_inclusive_jet_mult'\
                % name, '', 15, -0.5, 14.5))
            self['dijet_mult'] = Histo(ROOT.TH1D('%s_dijet_mult' % name, '', \
                15, -0.5, 14.5))
            self['true_jet_pt'] = Histo(ROOT.TH1D('%s_true_jet_pt' % name, '', 
                200, 0.0, 50.0))
            self['jet_ptw'] = Histo(ROOT.TH1D('%s_jet_ptw' % name, '', \
                200, 0.0, 50.0))
            
        
        self.tracks_plus = TrackHistogramCollection('%s_plus' % (name,), \
            tfile, keys)
        self.tracks_minus = TrackHistogramCollection('%s_minus' % (name,), \
            tfile, keys)
        self.tracks_sum = TrackHistogramCollection('%s_sum' % (name,), \
            tfile, keys)
    
    
    def fillEvent(self, event, ecuts):
        vertex = event.vertex(0)
        self['nVertices'].Fill(event.nVertices())
        self['bbc'].Fill(event.bbcTimeBin())
        
        if ecuts.vertex:
            self['vz'].Fill(vertex.z())
            
        if ecuts.vertex and ecuts.bbc:
            self['vx_vy'].Fill(vertex.x(), vertex.y())
            self['vzBBC'].Fill(vertex.z())
            if not simu:
                self['spinBit'].Fill(event.spinBit())
                self['bx7'].Fill(event.bx7())
        
        if simu:
            self['cosTheta'].Fill(event.cosTheta())
            self['hardP'].Fill(event.hardP())
            self['x1'].Fill(event.x1())
            self['x2'].Fill(event.x2())
            self['x1_x2'].Fill(event.x1(), event.x2())
    
    
    def fillJets(self, triggerJet, awayJet=None):
        pt = triggerJet.Pt()
        self['jet_pt'].Fill(pt)
        self['true_jet_pt'].Fill(shifted(pt))
        self['jet_ptw'].Fill(pt, minbias_jet_weight(pt))
        if awayJet is not None:
            self['jet_pt_balance'].Fill(pt, awayJet.Pt())
            self['jet_p_balance'].Fill(triggerJet.P(), awayJet.P())
            self['jet_eta_balance'].Fill(triggerJet.Eta(), awayJet.Eta())
            self['jet_phi_balance'].Fill(triggerJet.Phi(), awayJet.Phi())
    
    
    def trackHistograms(self,charge):
        if charge == 1:  return self.tracks_plus
        if charge == -1: return self.tracks_minus
        if charge == 0:  return self.tracks_sum
        return None
    
    
    def histos(self):
        val = self.values()
        return [v for v in val if v is not None] + self.tracks_plus.histos() \
            + self.tracks_minus.histos() + self.tracks_sum.histos()
    
    
    def Add(self, other):
        [ h.Add(other[key].h) for key, h in self.items() ]
        self.tracks_plus.Add( other.tracks_plus )
        self.tracks_minus.Add( other.tracks_minus )
        self.tracks_sum.Add( other.tracks_sum )
    
    
    def Flush(self):
        [ h.Flush() for h in self.values() ]
        self.tracks_plus.Flush()
        self.tracks_minus.Flush()
        self.tracks_sum.Flush()
    
    
    def Write(self):
        """make it persistent"""
        [ h.Write() for h in self.values() ]
        self.tracks_plus.Write()
        self.tracks_minus.Write()
        self.tracks_sum.Write()
    
    


class HistogramManager(dict):
    """
    generates histograms from StChargedPionEvent trees and reads them back 
    from disk
    """
    spinKeys = {5:'uu', 6:'du', 9:'ud', 10:'dd'}
    trigSetups = ('96011','96201','96211','96221','96233','hightower',
        'jetpatch', 'alltrigs','117001','137221','137222','137611','137622')
    
    def __init__(self, tfile=None, keys=None, triggers=None):
        super(HistogramManager, self).__init__()
        
        self.tfile = tfile
        
        if tfile is not None:
            self.name = os.path.basename(tfile.GetName())
            self.eventCounter = tfile.Get('eventCounter')
        else:
            self.eventCounter = ROOT.TH1I('eventCounter','',1,-1,1)
        
        self.fill = 0
        
        self.allHistos = []
        self.mytriggers = []
        
        if simu:
            #spinlist becomes a list of subprocesses
            self.spinlist = ('gg','qg','qq','other','anyspin')
            for spin in self.spinlist:
                self[spin] = {}
                setattr(self, spin, self[spin])
                for trig in self.trigSetups:
                    if triggers is None or trig in triggers:
                        self.mytriggers.append(trig)
                        self.allHistos.append( HistogramCollection('_%s_%s' \
                            % (trig, spin), tfile, keys) )
                        self[spin][trig] = self.allHistos[-1]
        else:
            self.spinlist = ('uu','ud','du','dd','other','anyspin')
            for spin in self.spinlist:
                self[spin] = {}
                setattr(self, spin, self[spin])
                for trig in self.trigSetups:
                    if triggers is None or trig in triggers:
                        self.mytriggers.append(trig)
                        self.allHistos.append( HistogramCollection('_%s_%s' \
                            % (trig, spin), tfile, keys) )
                        self[spin][trig] = self.allHistos[-1]
        self.mytriggers = analysis.uniqify(self.mytriggers)
    
    
    def processEvent(self, ev):
        """fill histograms with info from event"""
        self.eventCounter.Fill(0)
        
        if sys.version_info[1] >= 4:
            sortedTracks =  sorted(ev.tracks(), \
                lambda t1,t2: t1.Pt()<t2.Pt() and 1 or -1)
        else:
            sortedTracks = [track for track in ev.tracks()]
            sortedTracks.sort(lambda t1,t2: t1.Pt()<t2.Pt() and 1 or -1)
        
        ## spin sorting
        spin = 'other'
        if not simu and ev.isSpinValid():
            try: spin = self.spinKeys[ev.spinBit()]
            except KeyError: pass
        elif simu:
            if ev.processId() == 68:
                spin = 'gg'
            elif ev.processId() == 28:
                spin = 'qg'
            elif ev.processId() == 11:
                spin = 'qq'
        
        ## trigger selection -- convert transverse trigger IDs to longitudinal
        triggerOk = {}
        t = triggerOk
        if simu and year == 2005:
            for trigId in (96011, 96201, 96211, 96221, 96233):
                triggerOk[str(trigId)] = ev.isSimuTrigger(trigId)
                triggerOk['%d_hw' % (trigId,)] = False
            t['hightower'] = t['96011'] and t['96201']
            t['jetpatch'] = t['96011'] and t['96221']
            t['alltrigs'] = t['hightower'] or t['jetpatch']
            activeTriggers = ('96011', '96201', '96211', '96221', '96233',
                'hightower', 'jetpatch', 'alltrigs')
        elif simu and year == 2006:
            for trigId in (117001, 137221, 137222, 137611, 137622):
                triggerOk[str(trigId)] = ev.isSimuTrigger(trigId)
                triggerOk['%d_hw' % (trigId,)] = False
            t['jetpatch'] = t['117001'] and t['137222']
            t['alltrigs'] = t['jetpatch'] or t['137611'] or t['137622']
            activeTriggers = ('117001', '137221', '137222', '137611', '137622', 
                'jetpatch', 'alltrigs')
        elif ev.runId() < 7000000:
            if ev.isPolLong():
                for trigId in (96011, 96201, 96211, 96221, 96233):
                    t[str(trigId)] = ev.isTrigger(trigId) and \
                        ev.isSimuTrigger(trigId)
                    t['%d_hw' % (trigId,)] = ev.isTrigger(trigId)
            elif ev.isPolTrans():
                for trigId in (106011, 106201, 106211, 106221, 106233):
                    t[str(trigId-10000)] = ev.isTrigger(trigId) \
                        and ev.isSimuTrigger(trigId)
                    t['%d_hw' % (trigId-10000,)]=ev.isTrigger(trigId)
            t['hightower'] = t['96011'] or t['96201'] or t['96211']
            t['jetpatch']  = t['96011'] or t['96221'] or t['96233']
            t['alltrigs']  = t['hightower'] or t['jetpatch']
            activeTriggers = ('96011','96201','96211','96221','96233',
                'hightower','jetpatch', 'alltrigs')
        else:
            t['117001'] = ev.isTrigger(117001) and ev.isSimuTrigger(117001)
            if ev.isPolLong():
                t['137611'] = ev.isTrigger(137611) and ev.isSimuTrigger(137611)
                t['137622'] = ev.isTrigger(137622) and ev.isSimuTrigger(137622)
                t['jetpatch'] = \
                    (ev.isTrigger(137221) and ev.isSimuTrigger(137221)) or \
                    (ev.isTrigger(137222) and ev.isSimuTrigger(137222))
            elif ev.isPolTrans():
                t['137611'] = ev.isTrigger(127611) and ev.isSimuTrigger(127611)
                t['137622'] = ev.isTrigger(127622) and ev.isSimuTrigger(127622)
                t['jetpatch']= ev.isTrigger(127221) and ev.isSimuTrigger(127221)
            t['alltrigs'] = t['117001'] or t['jetpatch'] or \
                            t['137611'] or t['137622']
            activeTriggers = ('117001', '137611', '137622', 'jetpatch', 
                'alltrigs')
          
        ## event-wise histograms
        ecuts = EventCuts(ev)
        for trig in activeTriggers:
            if triggerOk[trig] and trig in self.mytriggers: 
                self[spin][trig].fillEvent(ev, ecuts)
        
        ## track-wise histograms
        if not ecuts.all: return
        tcuts = TrackCuts(self.fill)
        for track in ev.tracks():
            tcuts.set(track)
            for trig in activeTriggers:
                if triggerOk[trig] and trig in self.mytriggers:
                    tcoll = self[spin][trig].trackHistograms(track.charge())
                    tcoll.fillTrack(track, tcuts)
                    if tcuts.all:
                        tcoll['vz'].Fill(ev.vertex(0).z())
                        if ev.runId() < 7000000:
                            tcoll['one'].Fill(0)
        
        if simu:
            for track in ev.matchedPairs():
                for trig in activeTriggers:
                    if not triggerOk[trig] or trig not in self.mytriggers: 
                        continue
                    tcoll = self[spin][trig].trackHistograms(track.charge())
                    tcoll.fillMatchedPair(track)
                    
        ## jet studies
        jcuts = [JetCuts(jet, ev) for jet in ev.jets()]
        for trig in activeTriggers:
            if not triggerOk[trig] or trig not in self.mytriggers: continue
            try:
                itrig = int(trig)
            except ValueError:
                if trig == 'jetpatch':
                    ## use lower-threshold triggers since they're a superset
                    if (not simu and ev.runId() < 7000000) or \
                       (simu and year == 2005):
                        itrig = 96221
                    else:
                        itrig = 137221
                else:
                    continue
            
            diJets = [] ## a diJet is (trigger jet, away-side jet)
            inclusiveJets = []
            monoJets = []
            
            for i,jet in enumerate(ev.jets()):
                if jcuts[i].eta and jcuts[i].rt and itrig in jcuts[i].trig:
                    inclusiveJets.append(jet)
                    
                    ## now look for away-side partner
                    diJetFound = False
                    for j,jet2 in enumerate(ev.jets()):
                        if jet2 is jet: continue
                        if jcuts[j].eta and jcuts[j].rt:
                            if jet.DeltaPhi(jet2) > 2.0:
                                diJets.append( (jet, jet2) )
                                self[spin][trig].fillJets(jet, jet2)
                                diJetFound = True
                    
                    if not diJetFound: 
                        monoJets.append(jet)
                        self[spin][trig].fillJets(jet, None)
            
            self[spin][trig]['inclusive_jet_mult'].Fill(len(inclusiveJets))
            self[spin][trig]['dijet_mult'].Fill(len(diJets))
            
            ## time to fill the correlation histos
            for track in ev.tracks():
                tcuts.set(track)
                tcoll = self[spin][trig].trackHistograms(track.charge())
                
                # WRONG!!! Looping over monoJets + diJets is NOT the same as
                # looping over inclusive jets
                for jet in monoJets:
                    tcoll.fillTrackJetPair(track, tcuts, jet)
                
                for triggerJet, awayJet in diJets:
                    tcoll.fillTrackJetPair(track, tcuts, triggerJet, awayJet)
            
            tp = self[spin][trig].tracks_plus
            tm = self[spin][trig].tracks_minus
            for t in(tp,tm):
                t['away_mult'].Fill(t.away_mult)
                t['near_mult'].Fill(t.near_mult)
                
            ## define "lead pT" as leading charged pion (+ OR -) on that side
            if tp.away_lead_pt > tm.away_lead_pt:
                tp['away_lead_pt'].Fill(tp.away_lead_pt)
            elif tm.away_lead_pt > 0.0:
                tm['away_lead_pt'].Fill(tm.away_lead_pt)
            
            if tp.near_lead_pt > tm.near_lead_pt:
                tp['near_lead_pt'].Fill(tp.near_lead_pt)
            elif tm.near_lead_pt > 0.0:
                tm['near_lead_pt'].Fill(tm.near_lead_pt)
            
            ## calculate leading z particle relative to each away-side jet
            for triggerJet, awayJet in diJets:
                lead = awayJet.leadingParticle()
                if lead.detectorId() == ROOT.kTpcId:
                    found_match = False
                    for track in ev.tracks():
                        if matched(track, lead):
                            tcuts.set(track)
                            coll = track.charge() > 0 and tp or tm
                            if tcuts.all:
                                coll['lead_matched'].Fill(track.Pt())
                            else:
                                coll['lead_cutfail'].Fill(track.Pt())
                            found_match = True
                            break
                    if not found_match:
                        coll = lead.charge() > 0 and tp or tm
                        coll['lead_nomatch'].Fill(lead.Pt())
                else:
                    self[spin][trig]['lead_neutral'].Fill(lead.Pt())
            
            for jet in inclusiveJets:
                if not (10.0 < jet.Pt() < 30.0): continue
                for track in sortedTracks:
                    tcuts.set(track)
                    c = (track.charge() > 0) and tp or tm
                    dphi = track.DeltaPhi(jet)
                    if tcuts.all:
                        c['dphi'].Fill(dphi)                    
                    if abs(dphi) > 2.0:
                        z_noshift = track.Pt()/jet.Pt()
                        z = track.Pt()/shifted(jet.Pt())
                        z_low = track.Pt()/shifted(jet.Pt(), 'low')
                        z_high = track.Pt()/shifted(jet.Pt(), 'high')
                        if tcuts.dca and tcuts.fit and tcuts.pid:
                            c['away2_eta'].Fill(track.eta())
                        if tcuts.eta and tcuts.fit and tcuts.pid:
                            c['away2_dcaG'].Fill(track.globalDca().mag())
                        if tcuts.eta and tcuts.dca and tcuts.pid:
                            c['away2_nHitsFit'].Fill(track.nHitsFit())
                        if tcuts.eta and tcuts.dca and tcuts.fit:
                            c['away2_nSigmaPion'].Fill(track.nSigmaPion())
                        if tcuts.eta and tcuts.dca and tcuts.fit \
                            and tcuts.pid_bg:
                            c['z_away2_bg'].Fill(z)
                        if tcuts.all:
                            c['z_noshift'].Fill(z_noshift)
                            c['z_away2'].Fill(z)
                            c['z_away2_low'].Fill(z_low)
                            c['z_away2_high'].Fill(z_high)
                            c['meanpt'].Fill(z, track.Pt())
                            c['meanjetpt'].Fill(z, jet.Pt())
                            if ev.runId() > 7000000:
                                c['one'].Fill(0)
                            
                            if trig == '117001':
                                low = shifted(jet.Pt(), 'low')
                                mid = shifted(jet.Pt())
                                high = shifted(jet.Pt(), 'high')
                                l = minbias_jet_weight_true(low)
                                w = minbias_jet_weight_true(mid)
                                h = minbias_jet_weight_true(high)
                            else:
                                l = w = h = 1.0
                            
                            var = year==2006 and z or track.Pt()
                            if not simu: continue
                            for a in asymKeys[:-1]:
                                num = mcasym.num(a, ev)
                                c[a].Fill(var, num)
                                c[a+'f'].Fill(var, num*w)
                                c[a+'w'].Fill(var, num*w)
                                c[a+'l'].Fill(var, num*l)
                                c[a+'h'].Fill(var, num*h)
                            denom = mcasym.denom('NLO', ev)
                            c['denom'].Fill(var, denom)
                            c['denomf'].Fill(var, denom*w)
                            c['denomw'].Fill(var, denom*w)
                            c['denoml'].Fill(var, denom*l)
                            c['denomh'].Fill(var, denom*h)
                        # break
            
            for jet in inclusiveJets:
                if not (10.0 < jet.Pt() < 30.0): continue
                used_bins_plus = []
                used_bins_minus = []
                for track in sortedTracks:
                    tcuts.set(track)
                    if tcuts.all and math.fabs(track.DeltaPhi(jet)) > 2.0:
                        bin = zbin(track.Pt()/jet.Pt())
                        if track.charge() > 0:
                            if bin not in used_bins_plus:
                                tp['z_away4'].Fill(track.Pt()/jet.Pt())
                                used_bins_plus.append(bin)
                        else:
                            if bin not in used_bins_minus:
                                tm['z_away4'].Fill(track.Pt()/jet.Pt())
                                used_bins_minus.append(bin)
            
            for jet, awayJ in diJets:
                if not (10.0 < jet.Pt() < 30.0): continue
                for track in sortedTracks:
                    tcuts.set(track)
                    if tcuts.all and math.fabs(track.DeltaPhi(jet)) > 2.0:
                        z = track.Vect().Dot(awayJ.Vect()) / awayJ.P()**2
                        if track.charge() > 0:
                            tp['z_away3'].Fill(z)
                        else:
                            tm['z_away3'].Fill(z)
                        break
            
            tp.Clear()
            tm.Clear()
        
        [ collection.Flush() for collection in self.allHistos ]
    
    
    def histos(self):
        h = []
        for spin in self.keys():
            for trig in self[spin].keys():
                h.extend(self[spin][trig].histos())
        return h
    
    
    def Write(self,):
        """make it persistent in the current tfile"""
        self.eventCounter.Write()
        
        if simu:
            spinlist = ('gg','qg','qq','other')
        else:
            spinlist = ('uu','ud','du','dd','other')
        
        ## create charge-summed histos
        for spin in spinlist:
            for trig in self.mytriggers:
                self[spin][trig].trackHistograms(0).Add( \
                    self[spin][trig].trackHistograms(1)  )
                self[spin][trig].trackHistograms(0).Add( \
                    self[spin][trig].trackHistograms(-1) )
        
        ## create spin-integrated histos
        if simu:
            for trig in self.mytriggers:
                self['anyspin'][trig].Add( self['gg'][trig] )
                self['anyspin'][trig].Add( self['qg'][trig] )
                self['anyspin'][trig].Add( self['qq'][trig] )
                self['anyspin'][trig].Add( self['other'][trig] )                
        else:
            for trig in self.mytriggers:
                self['anyspin'][trig].Add( self['uu'][trig] )
                self['anyspin'][trig].Add( self['ud'][trig] )
                self['anyspin'][trig].Add( self['du'][trig] )
                self['anyspin'][trig].Add( self['dd'][trig] )
                self['anyspin'][trig].Add( self['other'][trig] )
        
        ## write everything to the current TFile
        [ collection.Write() for collection in self.allHistos ]
    


def writeHistograms(treeDir='~/data/run5/tree', globber='*', trigList=None):
    import analysis
    chain = ROOT.TChain('tree')
    if simu:
        chain.Add(treeDir + '/' + globber + '.root')
    else:
        chain.Add(treeDir + '/chargedPions_' + globber + '.tree.root')
    entries = chain.GetEntries()
    
    chain.GetEntry(0)
    fname = chain.GetCurrentFile().GetName()
    if simu:
        outname = os.path.basename(fname)[:9] + '.cphist.root'
    else:
        outname = os.path.basename(fname).replace('.tree.','.hist.')
    outFile = ROOT.TFile(outname, 'recreate')
    h = HistogramManager(triggers=trigList)
    if not simu: h.fill = analysis.getFill(analysis.getRun(fname))
    
    for i in xrange(entries):
        chain.GetEntry(i)
        
        ## found a new runnumber
        if fname != chain.GetCurrentFile().GetName():
            fname = chain.GetCurrentFile().GetName()
            if simu:
                print 'starting', fname
            else:
                outFile.cd()
                print 'saving', outname
                h.Write()
                outFile.Close()
                outname = os.path.basename(fname).replace('.tree.','.hist.')
                outFile = ROOT.TFile(outname, 'recreate')
                h = HistogramManager(triggers=trigList)
                h.fill = analysis.getFill(analysis.getRun(fname))
            
        h.processEvent(chain.event)
    
    print 'saving', outname
    outFile.cd()
    h.Write()
    outFile.Close()


def matched(t1, t2):
    return math.fabs(t1.Pt() - t2.Pt()) < 1e-3 and \
           math.fabs(t1.Eta() - t2.Eta()) < 1e-3 and \
           math.fabs(t1.Phi() - t2.Phi()) < 1e-3 and \
           t1.nHits() == t2.nHits() and \
           t1.nHitsFit() == t2.nHitsFit() and \
           t1.nHitsPoss() == t2.nHitsPoss() and \
           t1.charge() == t2.charge()


def zbin(zval):
    """
    return bin index for this track -- so that we take at most 1 track/bin/jet
    """
    zmax = [0.1, 0.2, 0.3, 0.4, 0.5, 0.7, 1.0]
    for i,z in enumerate(zmax):
        if zval < z: return i+1


def distortedPt(track):
    """
    correct track momentum in simulations for space charge distortion
    """
    A = 8.597E-04
    pT = track.Pt()
    if not simu: return pT
    if track.charge() > 0:
        return (pT - A*pT*pT)
    else:
        return (pT + A*pT*pT)


def shifted(jetpt, opt=None):
    """applies pT shift to correct measured jet pT back to particle level"""
    if year == 2006:
        if opt == 'low':
            return 1.201072 + 0.804055*jetpt - 0.001738157*jetpt*jetpt
        elif opt == 'high':
            return 1.874928 + 0.8837450*jetpt - 0.001643843*jetpt*jetpt
        else:
            ## /STAR/node/12022 for details
            return 1.538 + 0.8439*jetpt - 0.001691*jetpt*jetpt
    else:
        return jetpt

def minbias_jet_weight(x):
    """
    reweights measured jet pt from minbias to mock up triggered spectrum
    """
    return 0.7247 - 0.1603*x + 0.01062*(x**2) - 0.0001758*(x**3)

def minbias_jet_weight_true(x):
    """
    reweights corrected jet pt from minbias to mock up triggered spectrum
    """
    return 1.149 - 0.2655*x + 0.01857*(x**2) - 0.0003445*(x**3)

def condenseIntoFills(histDir, useLSF=False,fillList=None):
    """uses hadd to make histogram files for each fill instead of each run"""
    import analysis
    allFiles = os.listdir(histDir)
    fill_runlists = {}
    runlist = []
    for fname in allFiles:
        if not fname.endswith('.root'): continue
        run = analysis.getRun(fname)
        runlist.append(run)
    
    tuples = analysis.getAllFills(runlist)
    for run, fill in tuples:
        if run not in runlist: continue
        ## temporary hacks
        ## http://www.star.bnl.gov/HyperNews-star/protected/get/starspin/3324.html
        if 6144002 <= run <= 6144029: fill = 7128
        if 6144041 <= run <= 6144042: fill = 7129        
        if 6145067 <= run <= 6145068: fill = 7136
        if 6146001 <= run <= 6146026: fill = 7138
        try:
             fill_runlists[fill].append(run)
        except KeyError:
             fill_runlists[fill] = [run]
    
    for fill, runlist in fill_runlists.items():
        #if fill in (7048, 7055, 7327): continue  ## no final polarizations
        #if fill not in (7127, 7128, 7129, 7134, 7136, 7138): continue
        if fillList is None or fill in fillList:
            cmd = 'hadd chargedPions_%d.hist.root ' % (fill,)
            if len(runlist) == 1:
                print 'no need for hadd here!', fill, runlist[0]
                os.system(\
                    'cp %s/chargedPions_%d.hist.root chargedPions_%d.hist.root'\
                     % (histDir, runlist[0], fill))
            else:
                for run in runlist:
                    cmd += '%s/chargedPions_%d.hist.root ' % (histDir,run)
                if useLSF:
                    cmd = 'bsub -q star_cas_short -e err/%d.err -o out/%d.out '\
                     % (fill,fill) + cmd
                os.system(cmd)


def bsub(treeDir, runList=None, trigList=None):
    import analysis
    """
    submits a single writeHistograms job to LSF for each ROOT file in treeDir
    """
    allfiles = os.listdir(treeDir)
    try:
        os.mkdir('out')
        os.mkdir('err')
    except OSError: pass
    for fname in allfiles:
        if not fname.endswith('.root'): continue
        run = analysis.getRun(fname)
        if runList is None or run in runList:
            os.sys.stderr.write('%s ' % run)
            os.system('bsub -q star_cas_short -e err/%s.err -o out/%s.out \
                python -c "import analysis; analysis.histos.writeHistograms\
                (\'%s\',globber=\'*%s*\', trigList=%s)"' \
                % (run, run, treeDir, run, trigList))
            time.sleep(0.2)


def condor(treeDir, runList=None, trigList=None):
    import analysis
    """
    submits a single writeHistograms job to Condor for each ROOT file in treeDir
    """
    allfiles = os.listdir(treeDir)
    try:
        os.mkdir('out')
        os.mkdir('err')
        os.mkdir('log')
    except OSError: 
        pass
        
    ## build the script that we will run -- note trick with sys.argv
    f = open('job.py', 'w')
    f.write('import sys\n')
    f.write('import analysis\n')
    f.write('analysis.histos.writeHistograms(\'%s\', globber=\'*%%s*\' %% \
        sys.argv[1], trigList=%s)\n' % (treeDir, trigList))
    
    ## build the submit.condor file
    f = open('submit.condor', 'w')
    f.write('executable = /usr/bin/python\n')
    f.write('getenv = True\n')
    f.write('notification = Error\n')
    f.write('notify_user = kocolosk@rcf.rhic.bnl.gov\n')
    f.write('universe = vanilla\n')
    f.write('stream_output = True\n')
    f.write('stream_error = True\n')
    f.write('transfer_executable = False\n\n')
    
    for fname in allfiles:
        if not fname.endswith('.root'): continue
        run = analysis.getRun(fname)
        if runList is None or run in runList:
            f.write('output = out/%s.out\n' % run)
            f.write('error = err/%s.err\n' % run)
            f.write('log = log/%s.condor.log\n' % run)
            f.write('arguments = %s/job.py %s\n' % (os.getcwd(), run))
            f.write('queue\n\n')
    
    f.close()
    
    ## and off we go
    os.system('condor_submit submit.condor')


def hadd_simu(histDir='./'):
    """groups files by dataset, then calls hadd on each"""
    files = os.listdir(histDir)
    prefixes = [os.path.basename(f)[:7] for f in files if f.endswith('.root')]
    uniq = sets.Set(prefixes)
    
    ## build the hadd.condor file
    f = open('hadd.condor', 'w')
    f.write('executable = hadd\n')
    f.write('getenv = True\n')
    f.write('notification = Error\n')
    f.write('notify_user = kocolosk@rcf.rhic.bnl.gov\n')
    f.write('universe = vanilla\n')
    f.write('stream_output = True\n')
    f.write('stream_error = True\n')
    f.write('transfer_executable = False\n\n')
    
    for elem in uniq:
        print elem
        f.write('output = hadd/%s.out\n' % elem)
        f.write('error = hadd/%s.err\n' % elem)
        f.write('log = hadd/%s.condor.log\n' % elem)
        f.write('arguments = %s.cphist.root %s/%s_*.root\n' % \
            (elem, os.getcwd(), elem))
        f.write('queue\n\n')
    
    f.close()
    
    ## and off we go
    os.system('condor_submit hadd.condor')


def condor_simu(treeDir, trigList=None, year=2006):
    """
    submits a single writeHistograms job to Condor for each *sample* in treeDir.
    combines condor() and hadd_simu() into one step to (hopefully) save time
    """
    import analysis
    allfiles = os.listdir(treeDir)
    try:
        os.mkdir('out')
        os.mkdir('err')
        os.mkdir('log')
    except OSError: 
        pass
        
    files = os.listdir(treeDir)
    prefixes = [os.path.basename(f)[:9] for f in files if f.endswith('.root')]
    uniq = sets.Set(prefixes)
    
    ## build the script that we will run -- note trick with sys.argv
    f = open('job.py', 'w')
    f.write('import sys\n')
    f.write('import analysis\n')
    f.write('analysis.histos.simu = True\n')
    f.write('analysis.histos.year = %d\n' % year)
    f.write('analysis.histos.writeHistograms(\'%s\', globber=\'*%%s*\' %% \
        sys.argv[1], trigList=%s)\n' % (treeDir, trigList))
    
    ## build the submit.condor file
    f = open('submit.condor', 'w')
    f.write('executable = /usr/bin/python\n')
    f.write('getenv = True\n')
    f.write('notification = Error\n')
    f.write('notify_user = kocolosk@rcf.rhic.bnl.gov\n')
    f.write('universe = vanilla\n')
    f.write('stream_output = True\n')
    f.write('stream_error = True\n')
    f.write('transfer_executable = False\n\n')
    
    ## add jobs for each simulation sample
    for sample in uniq:
        f.write('output = out/%s.out\n' % sample)
        f.write('error = err/%s.err\n' % sample)
        f.write('log = log/%s.condor.log\n' % sample)
        f.write('arguments = %s/job.py %s\n' % (os.getcwd(), sample))
        f.write('queue\n\n')
    
    f.close()
    
    ## and off we go
    os.system('condor_submit submit.condor')

