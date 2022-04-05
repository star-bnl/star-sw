#!/usr/bin/env python
# encoding: utf-8
"""
minimc.py

Created by Adam Kocoloski on 2007-05-21.
Copyright (c) 2007 __MyCompanyName__. All rights reserved.
"""

import os, sys, getopt, math, re, glob
from array import array

import ROOT

help_message = '''
This module manages histogramming minimc.root files and combining the results.  Requires an
StJetSkimEvent(?) tree as input that contains the events of interest (i.e. trigger maker results).
'''

loadLibs = ''
site = 'local'

miniKeys = ['minbias', '96201', '96211', '96221', '96233', 'notrig']
trigKeys = miniKeys

asymKeys = ['lo','nlo','max','min','zero','m015','m030','m045','m060','m075','m090','m105','p030','p045','p060','p070']
asymKeys2 = ['p060','p045','p030','nlo','zero','m015','m030','m045','m060','m075','m090','m105','min']

Category = { 'MC':0, 'MATCHED':1, 'MERGED':2, 'SPLIT':3, 'CONTAM':4, 'GHOST':5, 'MATGLOB':6 }

processIds = { 'gg':68, 'qg':28, 'qq':11 }

xsec = { 
'2_3'       : 8.150,
'3_4'       : 1.302,
'4_5'       : 3.158E-01,
'5_7'       : 1.372E-01,
'7_9'       : 2.290E-02,
'9_11'      : 5.495E-03,
'11_15'     : 2.220E-03,
'15_25'     : 3.907E-04,
'25_35'     : 1.074E-05,
'above_35'  : 5.300E-07,
'45_55'     : 2.857E-08,
'55_65'     : 1.451E-09
}

#xsec = { 
#'3_4'      : 1.287, 
#'4_5'      : 3.117*10**-1, 
#'5_7'      : 1.360*10**-1, 
#'7_9'      : 2.305*10**-2, 
#'9_11'     : 5.494*10**-3, 
#'11_15'    : 2.228*10**-3, 
#'15_25'    : 3.895*10**-4, 
#'25_35'    : 1.016*10**-5, 
#'above_35' : 5.299*10**-7, 
#'45_55'    : 2.830*10**-8, 
#'55_65'    : 1.433*10**-9
#}
        
fileid = { 
'2_3'      : 1231,
'3_4'      : 1232,
'4_5'      : 1233, 
'5_7'      : 1224,
'7_9'      : 1225,
'9_11'     : 1226, 
'11_15'    : 1227, 
'15_25'    : 1228, 
'25_35'    : 1229,
'above_35' : 1230,
'45_55'    : 1270,
'55_65'    : 1271 
}
          
daveName = { 
'2_3'      : 'pt2',
'3_4'      : 'pt3',
'4_5'      : 'pt4',
'5_7'      : 'pt5',
'7_9'      : 'pt7',
'9_11'     : 'pt9',
'11_15'    : 'pt11',
'15_25'    : 'pt15',
'25_35'    : 'pt25',
'above_35' : 'pt35above',
'45_55'    : 'pt45',
'55_65'    : 'pt55'
}


def initForXgrid():
    try:
        dirList = os.listdir('/Volumes/star1.lns.mit.edu')
        if len(dirList) == 0: 
            os.system('/sbin/mount_nfs star1.lns.mit.edu:/XgridLibs /Volumes/star1.lns.mit.edu')
    except OSError:
        os.mkdir('/Volumes/star1.lns.mit.edu')
        os.system('/sbin/mount_nfs star1.lns.mit.edu:/XgridLibs /Volumes/star1.lns.mit.edu')
        
    if sys.byteorder == 'big': sys.path.append('/Volumes/star1.lns.mit.edu/sw_ppc/lib/root')
    else:   sys.path.append('/Volumes/star1.lns.mit.edu/sw/lib/root')
    
    try:
        dirList = os.listdir('/Volumes/deltag5.lns.mit.edu')
        if len(dirList) == 0:
            os.system('/sbin/mount_afp -o nobrowse "afp://;AUTH=No%20User%20Authent@deltag5.lns.mit.edu/scratch" /Volumes/deltag5.lns.mit.edu')
    except OSError:
        os.mkdir('/Volumes/deltag5.lns.mit.edu')
        os.system('/sbin/mount_afp -o nobrowse "afp://;AUTH=No%20User%20Authent@deltag5.lns.mit.edu/scratch" /Volumes/deltag5.lns.mit.edu')
    
    from socket import gethostname
    
    print 'xgrid initialization complete'
    print 'minimc.py executing on', gethostname()


class MiniMcHistos(dict):
    ptBins      = [40, 0, 20]
    etaBins     = [40, -2.0, 2.0]
    phiBins     = [20, -math.pi, math.pi]
    vzBins      = [300, -150., 150.]
    nFitBins    = [45, 0.5, 45.5]
    pBins       = [250, 0.2, 20.0]
    dEdxBins    = [100, 4., 40.]
    dcaGBins    = [100, 0., 3.1]
    partonBins  = [70, 0, 70]
    eventPartonBins = [1400, 0, 70]
    xbins       = [100, 0, 1]
    
    ggRescaleFactor = 1.15
    
    def __init__(self,trigIdString,subProcess='',tfile=None):
        self.trigIdString = trigIdString
        #self.subProcess = subProcess
        if subProcess != '':
            self.processId = processIds[subProcess]
        else:
            self.processId = 0
        
        self['pt'] = ROOT.TH1D('_%s_pt_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['ptRescaled'] = ROOT.TH1D('_%s_ptRescaled_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['eta'] = ROOT.TH1D('_%s_eta_%s' % (trigIdString,subProcess), '', self.etaBins[0], self.etaBins[1], self.etaBins[2])
        self['phi'] = ROOT.TH1D('_%s_phi_%s' % (trigIdString,subProcess), '', self.phiBins[0], self.phiBins[1], self.phiBins[2])
        self['vz'] = ROOT.TH1D('_%s_vz_%s' % (trigIdString,subProcess), '', self.vzBins[0], self.vzBins[1], self.vzBins[2])
        self['nHitsFit'] = ROOT.TH1D('_%s_nHitsFit_%s' % (trigIdString,subProcess), '', self.nFitBins[0], self.nFitBins[1], self.nFitBins[2])
        self['dEdx'] = ROOT.TH2D('_%s_dEdx_%s' % (trigIdString,subProcess), '', self.pBins[0], self.pBins[1], self.pBins[2], self.dEdxBins[0], self.dEdxBins[1], self.dEdxBins[2])
        self['dcaG'] = ROOT.TH1D('_%s_dcaG_%s' % (trigIdString,subProcess), '', self.dcaGBins[0], self.dcaGBins[1], self.dcaGBins[2])
        
        self['eventPartonicPt'] = ROOT.TH1D('_%s_eventPartonicPt_%s' % (trigIdString,subProcess), '', self.eventPartonBins[0], self.eventPartonBins[1], self.eventPartonBins[2])
        
        self['x1'] = ROOT.TH1D('_%s_x1_%s' % (trigIdString,subProcess), '', self.xbins[0], self.xbins[1], self.xbins[2])
        
        self['x2'] = ROOT.TH1D('_%s_x2_%s' % (trigIdString,subProcess), '', self.xbins[0], self.xbins[1], self.xbins[2])
        
        self['partonicPt'] = ROOT.TH2D('_%s_partonicPt_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2], self.partonBins[0], self.partonBins[1], self.partonBins[2])
        
        self['partonProf'] = ROOT.TProfile('_%s_partonProf_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        
        self['ptMc_ptPr'] = ROOT.TH2D('_%s_ptMc_ptPr_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2], self.ptBins[0], self.ptBins[1], self.ptBins[2])
        
        self['lo'] = ROOT.TH1D('_%s_lo_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['nlo'] = ROOT.TH1D('_%s_nlo_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['max'] = ROOT.TH1D('_%s_max_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['min'] = ROOT.TH1D('_%s_min_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['zero'] = ROOT.TH1D('_%s_zero_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['m015'] = ROOT.TH1D('_%s_m015_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['m030'] = ROOT.TH1D('_%s_m030_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['m045'] = ROOT.TH1D('_%s_m045_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['m060'] = ROOT.TH1D('_%s_m060_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['m075'] = ROOT.TH1D('_%s_m075_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['m090'] = ROOT.TH1D('_%s_m090_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['m105'] = ROOT.TH1D('_%s_m105_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['p030'] = ROOT.TH1D('_%s_p030_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['p045'] = ROOT.TH1D('_%s_p045_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['p060'] = ROOT.TH1D('_%s_p060_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['p070'] = ROOT.TH1D('_%s_p070_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        self['denom'] = ROOT.TH1D('_%s_denom_%s' % (trigIdString,subProcess), '', self.ptBins[0], self.ptBins[1], self.ptBins[2])
        
        self.clear()
        
        if tfile is not None:
            for key in self.keys():
                self[key].Delete()
                self[key] = tfile.Get('_%s_%s_%s' % (self.trigIdString,key,subProcess))
    
    
    def clear(self):
        self.miniEvent  = 0
        self.pythiaEvent= 0
        
        self.subProcess = 0
        self.partonicPt = 0
        
        self.lo         = 0
        self.nlo        = 0
        self.max        = 0
        self.min        = 0
        self.zero       = 0
        self.m015       = 0
        self.m030       = 0
        self.m045       = 0
        self.m060       = 0
        self.m075       = 0
        self.m090       = 0
        self.m105       = 0
        self.p030       = 0
        self.p045       = 0
        self.p060       = 0
        self.p070       = 0
    
    
    def fillEvent(self, miniEvent, pythiaEvent):
        import ROOT
        self.miniEvent = miniEvent
        self.pythiaEvent = pythiaEvent
        
        if self.processId != 0 and self.processId != self.pythiaEvent.processId(): return
        
        self.subProcess    = pythiaEvent.processId()
        self.partonicPt    = pythiaEvent.pt()
        self.lo            = pythiaEvent.ALL(ROOT.StPythiaEvent.LO)
        self.nlo           = pythiaEvent.ALL(ROOT.StPythiaEvent.NLO)
        self.max           = pythiaEvent.ALL(ROOT.StPythiaEvent.MAX)
        self.min           = pythiaEvent.ALL(ROOT.StPythiaEvent.MIN)
        self.zero          = pythiaEvent.ALL(ROOT.StPythiaEvent.ZERO)
        self.m015          = pythiaEvent.ALL(ROOT.StPythiaEvent.M015)
        self.m030          = pythiaEvent.ALL(ROOT.StPythiaEvent.M030)
        self.m045          = pythiaEvent.ALL(ROOT.StPythiaEvent.M045)
        self.m060          = pythiaEvent.ALL(ROOT.StPythiaEvent.M060)
        self.m075          = pythiaEvent.ALL(ROOT.StPythiaEvent.M075)
        self.m090          = pythiaEvent.ALL(ROOT.StPythiaEvent.M090)
        self.m105          = pythiaEvent.ALL(ROOT.StPythiaEvent.M105)
        self.p030          = pythiaEvent.ALL(ROOT.StPythiaEvent.P030)
        self.p045          = pythiaEvent.ALL(ROOT.StPythiaEvent.P045)
        self.p060          = pythiaEvent.ALL(ROOT.StPythiaEvent.P060)
        self.p070          = pythiaEvent.ALL(ROOT.StPythiaEvent.P070)
        
        self['vz'].Fill(miniEvent.vertexZ())
        self['eventPartonicPt'].Fill( self.partonicPt )
        self['x1'].Fill( pythiaEvent.x1() )
        self['x2'].Fill( pythiaEvent.x2() )
    
    
    def fillTrack(self,track):
        
        if self.processId != 0 and self.processId != self.pythiaEvent.processId(): return
        
        if self.pythiaEvent.processId() == 68:
           mypt = track.ptPr() ** self.ggRescaleFactor
        else:
           mypt = track.ptPr()
        
        
        ptCut  = mypt > 2.0
        pidCut = ( track.geantId() == 8 ) or ( track.geantId() == 9 )
        etaCut = math.fabs( track.etaPr() ) < 1.0
        dcaCut = math.fabs( track.dcaGl() ) < 1.0
        hitCut = ( track.fitPts() ) > 25
        vzCut  = ( math.fabs( self.miniEvent.vertexZ() ) < 60.0 ) and ( math.fabs( self.miniEvent.mcVertexZ() ) < 60 )
        
        if ptCut  and pidCut and etaCut and dcaCut and hitCut and vzCut:
           self['phi'].Fill(track.phiPr())
        
        if pidCut and etaCut and dcaCut and hitCut and vzCut:
            self['pt'].Fill(track.ptPr())
            self['ptRescaled'].Fill(mypt)
        
        if ptCut  and pidCut and dcaCut and hitCut and vzCut:
            self['eta'].Fill(track.etaPr())
            
        if ptCut  and pidCut and etaCut and dcaCut and vzCut:
            self['nHitsFit'].Fill(track.fitPts())
            
        if ptCut  and etaCut and dcaCut and hitCut and vzCut:
            self['dEdx'].Fill(track.pPr(), track.dedx() * 1e7)
            
        if ptCut  and pidCut and etaCut and hitCut and vzCut:
            self['dcaG'].Fill(track.dcaGl())
        
        if ptCut  and pidCut and etaCut and dcaCut and hitCut and vzCut:
            self['partonicPt'].Fill(mypt, self.partonicPt)
            self['partonProf'].Fill(mypt, self.partonicPt)
            
            self['ptMc_ptPr'].Fill(track.ptMc(),mypt)
            
            self['lo'].Fill(mypt,self.lo)
            self['nlo'].Fill(mypt,self.nlo)
            self['max'].Fill(mypt,self.max)
            self['min'].Fill(mypt,self.min)
            self['zero'].Fill(mypt,self.zero)
            self['m015'].Fill(mypt,self.m015)
            self['m030'].Fill(mypt,self.m030)
            self['m045'].Fill(mypt,self.m045)
            self['m060'].Fill(mypt,self.m060)
            self['m075'].Fill(mypt,self.m075)
            self['m090'].Fill(mypt,self.m090)
            self['m105'].Fill(mypt,self.m105)
            self['p030'].Fill(mypt,self.p030)
            self['p045'].Fill(mypt,self.p045)
            self['p060'].Fill(mypt,self.p060)
            self['p070'].Fill(mypt,self.p070)
            self['denom'].Fill(mypt)
    
    
    def add(self, other, weight, nevents, minCounts):
        originalMinCounts = minCounts
        
        for key in self.keys():
            
            #if key in ('ptMc_ptPr',): minCounts = 0
            #else: minCounts = originalMinCounts
            
            #asymCheck = key.split('_')[0]
            if key in asymKeys or key == 'denom': continue
            
            if key not in other.keys(): continue
            
            nbins = self[key].GetBin(self[key].GetNbinsX(),self[key].GetNbinsY(),self[key].GetNbinsZ())
            for i in range(nbins):
                content = self[key].GetBinContent(i+1)
                error   = self[key].GetBinError(i+1)**2
                if other[key] is None: continue
                nparticles = other[key].GetBinContent(i+1)
                if nparticles >= minCounts:
                    wp = weight / nevents
                    content += nparticles * wp
                    error   += wp * wp * nparticles * (1+nparticles/nevents)
                self[key].SetBinContent(i+1,content)
                self[key].SetBinError(i+1, math.sqrt(error))
    
    
    def draw(self):
        c1 = ROOT.TCanvas('c1','The Basics', 0, 0, 1000, 800)
        c1.Divide(4,3)
        
        basicKeys = ['pt','eta','phi','vz','nHitsFit','dEdx','dcaG','partonicPt','partonProf','ptMc_ptPr']
        for index, key in enumerate(basicKeys):
            c1.cd(index+1)
            self[key].Draw()
        
        c1.cd(1).SetLogy()
        c1.cd(2).SetLogy()
        c1.cd(5).SetLogy()
        c1.cd(6).SetLogy()
        c1.cd(6).SetLogx()
        c1.cd(7).SetLogy()
        
        c2 = ROOT.TCanvas('c2', 'Asymmetries', 0, 0, 1000, 800)
        c2.Divide(4,4)
        for index, key in enumerate(asymKeys):
            c2.cd(index+1)
            self[key].Draw()
            
        c3 = ROOT.TCanvas('c3', 'g-g Asymmetries', 0, 0, 1000, 800)
        c3.Divide(4,4)
        for index, key in enumerate(asymKeys):
            key = '%s_gg' % (key,)
            c3.cd(index+1)
            self[key].Draw()
        
        c4 = ROOT.TCanvas('c4', 'q-g Asymmetries', 0, 0, 1000, 800)
        c4.Divide(4,4)
        for index, key in enumerate(asymKeys):
            key = '%s_qg' % (key,)
            c4.cd(index+1)
            self[key].Draw()
        
        c5 = ROOT.TCanvas('c5', 'q-q Asymmetries', 0, 0, 1000, 800)
        c5.Divide(4,4)
        for index, key in enumerate(asymKeys):
            key = '%s_qg' % (key,)
            c5.cd(index+1)
            self[key].Draw()
            
        raw_input('what do you think?')
    



def accept_mc_track(track):
    geantId = track.geantId()
    
    if (geantId != 8) and (geantId != 9):   return False
    if math.fabs(track.etaMc()):            return False
    if track.nHitMc() < 25:                 return False
    
    return True


def accept_reco_track(track):
    geantId = track.geantId()
    
    if (geantId != 8) and (geantId != 9):   return False
    if math.fabs(track.etaPr()) > 1.:       return False
    if math.fabs(track.dcaGl()) > 1.:       return False
    if track.fitPts() < 25:                 return False
    
    return True


def accept_event(ev):
    if math.fabs(ev.vertexZ()) > 60:        return False
    if math.fabs(ev.mcVertexZ()) > 60:      return False
    
    return True


def makeTriggerNtuple2(inputFileList):
    """ntuple contains event-level info from Renee's old files"""
    import ROOT
    ROOT.gSystem.Load('StMiniMcEvent')
    
    fileid = {'pt3':1232, 'pt4':1233, 'pt5':1224, 'pt7':1225, 'pt9':1226, 'pt11':1227, 'pt15':1228, 'pt25':1229, 'pt35':1230, 'pt45':1270, 'pt55':1271}
    
    outFile = ROOT.TFile('trigs.nt.root','recreate')
    nt = ROOT.TNtuple('nt','trigger simu results','fileid1:fileid2:event:subProcess:isMB:isHT1:isHT2:isJP1:isJP2:lo:nlo:max:min:zero:partonicPt')
    
    for path in inputFileList:
        print 'now skimming', path
        tfile = ROOT.TFile(path,'read')
        tree = tfile.Get('Event')
        
        #parse the path for fileid1 and fileid2
        fileid1 = fileid[re.search('pt\d{1,2}',path).group()]
        fileid2 = int(re.search('_\d{2,3}',path).group().strip('_'))
        
        for i in xrange(tree.GetEntries()):
            tree.GetEntry(i)
            nt.Fill(fileid1,fileid2,tree.evtID,tree.pID,tree.bbc,tree.HT1_2005,tree.HT2_2005,tree.JP1_2005,tree.JP2_2005,
                tree.weight_LO,tree.weight_NLO,tree.weight_NLO_gmax,tree.weight_NLO_gmin,tree.weight_NLO_g0,tree.hard_p)
        tfile.Close()
    
    outFile.cd()
    nt.Write()
    outFile.Close()


def makeTriggerNtuple(inputFileList):
    """ntuple contains event-level info from my old TChargePion trees."""
    outFile = ROOT.TFile('simutrigs.nt.root','recreate')
    nt = ROOT.TNtuple('nt','trigger simu results','fileid1:fileid2:run:event:subProcess:isMB:isHT1:isHT2:isJP1:isJP2')
    
    ev = ROOT.TChargedPionEvent()
    
    for path in inputFileList:
        print 'now skimming', path
        tfile = ROOT.TFile(path,'read')
        pionTree = tfile.Get('pionTree')
        pionTree.SetBranchAddress('Event Branch',ev)
        for i in xrange(pionTree.GetEntries()):
            pionTree.GetEntry(i)
            nt.Fill(ev.fileid1,ev.fileid2,ev.event,ev.run,ev.subProcessID,ev.bbcTrigger,ev.ht1TrigMaker[0],ev.ht2TrigMaker[0],ev.jp1TrigMaker[0],ev.jp2TrigMaker[0])
        tfile.Close()
    
    outFile.cd()
    nt.Write()
    outFile.Close()


def fill(sample, fileLimit = None, charge = 0):
    """fill histograms using StMiniMcEvent and custom trigger Ntuple as inputs"""
    if site == 'Xgrid':
        inputFileList = glob.glob('/Volumes/deltag5.lns.mit.edu/common/pythia/P05ih/minimc/%s/*' % (sample,))
        trigFile = ROOT.TFile('/Volumes/deltag5.lns.mit.edu/kocolosk/pythia/trigs.nt.root','read')
    else:
        inputFileList = glob.glob('/Volumes/scratch/common/pythia/P05ih/minimc/%s/*' % (sample,))
        trigFile = ROOT.TFile('/Volumes/scratch/kocolosk/pythia/trigs.nt.root','read')
        
    if fileLimit is not None: inputFileList = inputFileList[:fileLimit]
    
    eventCounter = 0
    
    #ntuple storing supplemental event info
    nt = trigFile.Get('nt')
    print 'building trigger index'
    nt.BuildIndex('fileid1*1000 + fileid2','event')
    
    miniHists = [ MiniMcHistos(key) for key in miniKeys ]
                    
    ev = ROOT.StMiniMcEvent()
    
    eventCounter = ROOT.TH1I('eventCounter','event counter',1,-0.5,0.5)
    
    for path in inputFileList:
        fileid1 = int(re.search('12\d{2}',path).group())
        fileid2 = int(re.search('_\d{2,3}_',path).group().strip('_'))
        
        print 'analyzing', fileid1, fileid2, path
        
        tfile = ROOT.TFile(path,'read')
        ttree = tfile.Get('StMiniMcTree')
        ttree.SetBranchAddress('StMiniMcEvent',ev)
        
        for i in xrange(ttree.GetEntries()):
            ttree.GetEntry(i)
            ret = nt.GetEntryWithIndex(fileid1*1000 + fileid2, ev.eventId())
            
            [m.clear() for m in miniHists]
            
            if ret > 0:
                for m in miniHists:
                    m.subProcess    = nt.subProcess
                    m.partonicPt    = nt.partonicPt
                    m.lo            = nt.lo
                    m.nlo           = nt.nlo
                    m.max           = nt.max
                    m.min           = nt.min
                    m.zero          = nt.zero
                    
                eventCounter.Fill(0.)
                
                if accept_event(ev) is False: continue
                
                miniHists[5].fillEvent(ev)
                
                if int(nt.isMB) > 0:    
                    miniHists[0].fillEvent(ev)
                    if int(nt.isHT1) > 0:   miniHists[1].fillEvent(ev)
                    if int(nt.isHT2) > 0:   miniHists[2].fillEvent(ev)
                    if int(nt.isJP1) > 0:   miniHists[3].fillEvent(ev)
                    if int(nt.isJP2) > 0:   miniHists[4].fillEvent(ev)
        
                #find all reco pions
                tracks = ev.tracks(Category['MATCHED'])
                for track in tracks:
                    if accept_reco_track(track):
                        
                        if charge != 0 and track.charge() != charge: continue
                        
                        miniHists[5].fillTrack(track)
                        
                        if int(nt.isMB) > 0:    
                            miniHists[0].fillTrack(track)
                            if int(nt.isHT1) > 0:   miniHists[1].fillTrack(track)
                            if int(nt.isHT2) > 0:   miniHists[2].fillTrack(track)
                            if int(nt.isJP1) > 0:   miniHists[3].fillTrack(track)
                            if int(nt.isJP2) > 0:   miniHists[4].fillTrack(track)
            else:
                print 'no matching ntuple entry for', fileid1, fileid2, ev.eventId()
    
    if charge == 0:     chargeName = 'sum'
    elif charge == 1:   chargeName = 'plus'
    elif charge == -1:  chargeName = 'minus'
    
    outFile = ROOT.TFile('%s.%s.hist.root' % (sample,chargeName),'recreate')
    eventCounter.Write()
    for m in miniHists: [h.Write() for h in m.values()]


def fill2(sample, fileLimit = None, useXrootd = False, listFile=None):
    """fill histograms using StJetSkimTree/StPythiaEvent and minimc files as inputs"""
    pathToMinimc  = '/Volumes/data01/reco/pp200/pythia6_205/%sgev/cdf_a/y2004y/gheisha_on/p05ih' % (sample,)
    pathToJetSkim = '/Volumes/data01/sim/staszak/2005jets/2005jets_15grids'
    
    ROOT.gSystem.Load('StJetMaker')
    ROOT.gSystem.Load('StMiniMcEvent')
    
    fileCounter = 0
    
    skimEvent = ROOT.StJetSkimEvent()
    miniEvent = ROOT.StMiniMcEvent()
    pythiaEvent = ROOT.StPythiaEvent()
    
    outFilePlus         = ROOT.TFile('%s.plus.hist.root' % (sample,),'recreate')
    miniHistsPlus       = [ MiniMcHistos(key,'') for key in miniKeys ]
    miniHistsPlus_gg    = [ MiniMcHistos(key,'gg') for key in miniKeys ]
    miniHistsPlus_qg    = [ MiniMcHistos(key,'qg') for key in miniKeys ]
    miniHistsPlus_qq    = [ MiniMcHistos(key,'qq') for key in miniKeys ]
    
    outFileMinus        = ROOT.TFile('%s.minus.hist.root' % (sample,),'recreate')
    miniHistsMinus      = [ MiniMcHistos(key,'') for key in miniKeys ]
    miniHistsMinus_gg   = [ MiniMcHistos(key,'gg') for key in miniKeys ]
    miniHistsMinus_qg   = [ MiniMcHistos(key,'qg') for key in miniKeys ]
    miniHistsMinus_qq   = [ MiniMcHistos(key,'qq') for key in miniKeys ]
    
    outFileSum          = ROOT.TFile('%s.sum.hist.root' % (sample,),'recreate')
    miniHistsSum        = [ MiniMcHistos(key,'') for key in miniKeys ]
    miniHistsSum_gg     = [ MiniMcHistos(key,'gg') for key in miniKeys ]
    miniHistsSum_qg     = [ MiniMcHistos(key,'qg') for key in miniKeys ]
    miniHistsSum_qq     = [ MiniMcHistos(key,'qq') for key in miniKeys ]
    
    miniHistLists = [
        miniHistsPlus, miniHistsPlus_gg, miniHistsPlus_qg, miniHistsPlus_qq,
        miniHistsMinus, miniHistsMinus_gg, miniHistsMinus_qg, miniHistsMinus_qq,
        miniHistsSum, miniHistsSum_gg, miniHistsSum_qg, miniHistsSum_qq,
    ]
    
    eventCounter    = ROOT.TH1I('eventCounter','event counter',1,-0.5,0.5)
    
    if listFile is not None:
        f = open(listFile)
        minimcPaths = [ line.strip() for line in f]
    else:
        minimcPaths = os.listdir(pathToMinimc)
        
    for fileName in minimcPaths:
        if not fileName.endswith('.root'): continue
        fileCounter += 1
        if fileLimit is not None and fileCounter > fileLimit: break
        
        print '%03d : %s' % (fileCounter, fileName)
        fileIndex = fileName.split('_')[-2]
        
        skimPath = os.path.join( pathToJetSkim, 'skim_%s_%s.root' % (daveName[sample], fileIndex) )
        if not os.path.isfile(skimPath) and not useXrootd: 
            print 'missing file = ', skimPath
            continue
        if useXrootd: skimPath = 'root://deltag5.lns.mit.edu/' + skimPath
        print skimPath
        skimFile = ROOT.TFile.Open( skimPath )
        skimTree = skimFile.Get('jetSkimTree')
        skimTree.SetBranchAddress('skimEventBranch', skimEvent)
        
        #if useXrootd:
        #    miniFile = ROOT.TFile.Open( 'root://deltag5.lns.mit.edu/' + os.path.join( pathToMinimc, fileName ) )
        #else:
        if useXrootd:
            miniFile = ROOT.TFile.Open( fileName )
        else:
            miniFile = ROOT.TFile.Open( os.path.join( pathToMinimc, fileName ) )
        miniTree = miniFile.Get('StMiniMcTree')
        miniTree.SetBranchAddress('StMiniMcEvent', miniEvent)
        
        
        print 'skim entries = ', skimTree.GetEntries()
        print 'mini entries = ', miniTree.GetEntries()
        
        [ eventCounter.Fill(0.) for i in range(skimTree.GetEntries()) ]
        
        # set pythia event now so we don't delete it every event
        skimTree.GetEntry(1)
        pythiaEvent = skimEvent.mcEvent()
        pythiaEvent.ResetBit( pythiaEvent.kMustCleanup ) # if we don't it will be deleted on GetEntry
        
        #assert(miniTree.GetEntries() == skimTree.GetEntries())
        skimTree.BuildIndex('mRunId', 'mEventId')
        
        for i in range(miniTree.GetEntries()):
            miniTree.GetEntry(i)
            skimTree.GetEntryWithIndex(miniEvent.runId(), miniEvent.eventId())
            
            assert(miniEvent.eventId() == skimEvent.eventId())
            
            for li in miniHistLists:
                [m.clear() for m in li]
                li[5].fillEvent(miniEvent, pythiaEvent)
            
            triggers = [ skimEvent.trigger(96201), 
                         skimEvent.trigger(96211), 
                         skimEvent.trigger(96221), 
                         skimEvent.trigger(96233) ]
            
            trigDidFire = [ skimEvent.eBbc() > 0 and skimEvent.wBbc() > 0 ]
            trigDidFire.extend( [ trig is not None and trig.shouldFire() > 0 for trig in triggers ] )
            
            for li in miniHistLists:
                if trigDidFire[0]:
                    li[0].fillEvent(miniEvent, pythiaEvent)
                    for i in range(4):
                        if trigDidFire[i+1]: 
                            li[i+1].fillEvent(miniEvent, pythiaEvent)
            
            tracks = miniEvent.tracks(Category['MATCHED'])
            for track in tracks:
                if track.charge() == 1:  
                    miniHistsPlus[5].fillTrack(track)                
                    miniHistsPlus_gg[5].fillTrack(track)                
                    miniHistsPlus_qg[5].fillTrack(track)                
                    miniHistsPlus_qq[5].fillTrack(track)                
                if track.charge() == -1: 
                    miniHistsMinus[5].fillTrack(track)                
                    miniHistsMinus_gg[5].fillTrack(track)                
                    miniHistsMinus_qg[5].fillTrack(track)                
                    miniHistsMinus_qq[5].fillTrack(track)                
                miniHistsSum[5].fillTrack(track)
                miniHistsSum_gg[5].fillTrack(track)
                miniHistsSum_qg[5].fillTrack(track)
                miniHistsSum_qq[5].fillTrack(track)
                
                if trigDidFire[0]: 
                    if track.charge() == 1:  
                        miniHistsPlus[0].fillTrack(track)                
                        miniHistsPlus_gg[0].fillTrack(track)                
                        miniHistsPlus_qg[0].fillTrack(track)                
                        miniHistsPlus_qq[0].fillTrack(track)                
                    if track.charge() == -1: 
                        miniHistsMinus[0].fillTrack(track)                
                        miniHistsMinus_gg[0].fillTrack(track)                
                        miniHistsMinus_qg[0].fillTrack(track)                
                        miniHistsMinus_qq[0].fillTrack(track)                
                    miniHistsSum[0].fillTrack(track)
                    miniHistsSum_gg[0].fillTrack(track)
                    miniHistsSum_qg[0].fillTrack(track)
                    miniHistsSum_qq[0].fillTrack(track)
                    for i in range(4):
                        if trigDidFire[i+1]: 
                            if track.charge() == 1:  
                                miniHistsPlus[i+1].fillTrack(track)
                                miniHistsPlus_gg[i+1].fillTrack(track)
                                miniHistsPlus_qg[i+1].fillTrack(track)
                                miniHistsPlus_qq[i+1].fillTrack(track)
                            if track.charge() == -1: 
                                miniHistsMinus[i+1].fillTrack(track)
                                miniHistsMinus_gg[i+1].fillTrack(track)
                                miniHistsMinus_qg[i+1].fillTrack(track)
                                miniHistsMinus_qq[i+1].fillTrack(track)
                            miniHistsSum[i+1].fillTrack(track)
                            miniHistsSum_gg[i+1].fillTrack(track)
                            miniHistsSum_qg[i+1].fillTrack(track)
                            miniHistsSum_qq[i+1].fillTrack(track)
            
        skimFile.Close()
        miniFile.Close()
    
    #miniHists[0].draw()
    #miniHists[-1].draw()
    
    outFilePlus.cd()
    eventCounter.Write()
    for m in miniHistsPlus: [h.Write() for h in m.values()]
    for m in miniHistsPlus_gg: [h.Write() for h in m.values()]
    for m in miniHistsPlus_qg: [h.Write() for h in m.values()]
    for m in miniHistsPlus_qq: [h.Write() for h in m.values()]
    outFilePlus.Close()
    
    outFileMinus.cd()
    eventCounter.Write()
    for m in miniHistsMinus: [h.Write() for h in m.values()]
    for m in miniHistsMinus_gg: [h.Write() for h in m.values()]
    for m in miniHistsMinus_qg: [h.Write() for h in m.values()]
    for m in miniHistsMinus_qq: [h.Write() for h in m.values()]
    outFileMinus.Close()
    
    outFileSum.cd()
    eventCounter.Write()
    for m in miniHistsSum: [h.Write() for h in m.values()]
    for m in miniHistsSum_gg: [h.Write() for h in m.values()]
    for m in miniHistsSum_qg: [h.Write() for h in m.values()]
    for m in miniHistsSum_qq: [h.Write() for h in m.values()]
    outFileSum.Close()


def fill3(minimcList):
    """fill histograms using StJetSkimTree/StPythiaEvent and minimc files as inputs"""
    #pathToMinimc  = '/Volumes/data01/reco/pp200/pythia6_205/%sgev/cdf_a/y2004y/gheisha_on/p05ih' % (sample,)
    #pathToJetSkim = '/Volumes/data01/sim/staszak/2005jets/2005jets_15grids'
    pathToJetSkim = '/star/data04/sim/staszak/2005jets/2005jets_15grids'
    
    libs_to_load = [ 'libPhysics', 'libTable', 'StarRoot', 'StarClassLibrary', 'St_base', 
    'StChain', 'St_Tables', 'StUtilities', 'StTreeMaker', 'StIOMaker', 'StTriggerDataMaker', 
    'StBichsel', 'StEvent', 'StEventUtilities', 'StDbLib', 'StEmcUtil', 'StTofUtil', 'StPmdUtil', 
    'StStrangeMuDstMaker', 'StMuDSTMaker', 'StDaqLib', 'StDetectorDbMaker', 'StEmcTriggerMaker', 
    'StMCAsymMaker', 'StSpinDbMaker', 'StJetFinder', 'StJetMaker', 'StChargedPionAnalysisMaker']
    
    print 'minimc : loading shared libraries ...'
    libs_already_loaded = ROOT.gSystem.GetLibraries()
    for library in libs_to_load:
       if library not in libs_already_loaded:
          ROOT.gSystem.Load(library)
          #print 'analysis : loaded', library
    print 'minimc : loading complete'
    
    #ROOT.gSystem.Load('StJetMaker')
    #ROOT.gSystem.Load('StMiniMcEvent')
    
    fileCounter = 0
    
    skimEvent = ROOT.StJetSkimEvent()
    miniEvent = ROOT.StMiniMcEvent()
    pythiaEvent = ROOT.StPythiaEvent()
    
    outFilePlus         = ROOT.TFile('%s.plus.hist.root' % (sample,),'recreate')
    miniHistsPlus       = [ MiniMcHistos(key,'') for key in miniKeys ]
    miniHistsPlus_gg    = [ MiniMcHistos(key,'gg') for key in miniKeys ]
    miniHistsPlus_qg    = [ MiniMcHistos(key,'qg') for key in miniKeys ]
    miniHistsPlus_qq    = [ MiniMcHistos(key,'qq') for key in miniKeys ]
    
    outFileMinus        = ROOT.TFile('%s.minus.hist.root' % (sample,),'recreate')
    miniHistsMinus      = [ MiniMcHistos(key,'') for key in miniKeys ]
    miniHistsMinus_gg   = [ MiniMcHistos(key,'gg') for key in miniKeys ]
    miniHistsMinus_qg   = [ MiniMcHistos(key,'qg') for key in miniKeys ]
    miniHistsMinus_qq   = [ MiniMcHistos(key,'qq') for key in miniKeys ]
    
    outFileSum          = ROOT.TFile('%s.sum.hist.root' % (sample,),'recreate')
    miniHistsSum        = [ MiniMcHistos(key,'') for key in miniKeys ]
    miniHistsSum_gg     = [ MiniMcHistos(key,'gg') for key in miniKeys ]
    miniHistsSum_qg     = [ MiniMcHistos(key,'qg') for key in miniKeys ]
    miniHistsSum_qq     = [ MiniMcHistos(key,'qq') for key in miniKeys ]
    
    miniHistLists = [
        miniHistsPlus, miniHistsPlus_gg, miniHistsPlus_qg, miniHistsPlus_qq,
        miniHistsMinus, miniHistsMinus_gg, miniHistsMinus_qg, miniHistsMinus_qq,
        miniHistsSum, miniHistsSum_gg, miniHistsSum_qg, miniHistsSum_qq,
    ]
    
    eventCounter    = ROOT.TH1I('eventCounter','event counter',1,-0.5,0.5)
    
    #if listFile is not None:
    #    f = open(listFile)
    #    minimcPaths = [ line.strip() for line in f]
    #else:
    #    minimcPaths = os.listdir(pathToMinimc)
    f = open(minimcList)
    minimcPaths = [ line.strip() for line in f ]
        
    for fileName in minimcPaths:
        baseName = os.path.basename(fileName)
        if not fileName.endswith('.root'): continue
        fileCounter += 1
        if fileLimit is not None and fileCounter > fileLimit: break
        
        print '%03d : %s' % (fileCounter, baseName)
        fileIndex = baseName.split('_')[-2]
        
        skimPath = os.path.join( pathToJetSkim, 'skim_%s_%s.root' % (daveName[sample], fileIndex) )
        if not os.path.isfile(skimPath) and not useXrootd: 
            print 'missing file = ', skimPath
            continue
        #if useXrootd: skimPath = 'root://deltag5.lns.mit.edu/' + skimPath
        print skimPath
        skimFile = ROOT.TFile.Open( skimPath )
        skimTree = skimFile.Get('jetSkimTree')
        skimTree.SetBranchAddress('skimEventBranch', skimEvent)
        
        #if useXrootd:
        #    miniFile = ROOT.TFile.Open( 'root://deltag5.lns.mit.edu/' + os.path.join( pathToMinimc, fileName ) )
        #else:
        #if useXrootd:
        #    miniFile = ROOT.TFile.Open( fileName )
        #else:
        #miniFile = ROOT.TFile.Open( os.path.join( pathToMinimc, fileName ) )
        miniFile = ROOT.TFile.Open( fileName )
        miniTree = miniFile.Get('StMiniMcTree')
        miniTree.SetBranchAddress('StMiniMcEvent', miniEvent)
        
        
        print 'skim entries = ', skimTree.GetEntries()
        print 'mini entries = ', miniTree.GetEntries()
        
        [ eventCounter.Fill(0.) for i in range(skimTree.GetEntries()) ]
        
        # set pythia event now so we don't delete it every event
        skimTree.GetEntry(1)
        pythiaEvent = skimEvent.mcEvent()
        pythiaEvent.ResetBit( pythiaEvent.kMustCleanup ) # if we don't it will be deleted on GetEntry
        
        #assert(miniTree.GetEntries() == skimTree.GetEntries())
        skimTree.BuildIndex('mRunId', 'mEventId')
        
        for i in range(miniTree.GetEntries()):
            miniTree.GetEntry(i)
            skimTree.GetEntryWithIndex(miniEvent.runId(), miniEvent.eventId())
            
            assert(miniEvent.eventId() == skimEvent.eventId())
            
            for li in miniHistLists:
                [m.clear() for m in li]
                li[5].fillEvent(miniEvent, pythiaEvent)
            
            triggers = [ skimEvent.trigger(96201), 
                         skimEvent.trigger(96211), 
                         skimEvent.trigger(96221), 
                         skimEvent.trigger(96233) ]
            
            trigDidFire = [ skimEvent.eBbc() > 0 and skimEvent.wBbc() > 0 ]
            trigDidFire.extend( [ trig is not None and trig.shouldFire() > 0 for trig in triggers ] )
            
            for li in miniHistLists:
                if trigDidFire[0]:
                    li[0].fillEvent(miniEvent, pythiaEvent)
                    for i in range(4):
                        if trigDidFire[i+1]: 
                            li[i+1].fillEvent(miniEvent, pythiaEvent)
            
            tracks = miniEvent.tracks(Category['MATCHED'])
            for track in tracks:
                if track.charge() == 1:  
                    miniHistsPlus[5].fillTrack(track)                
                    miniHistsPlus_gg[5].fillTrack(track)                
                    miniHistsPlus_qg[5].fillTrack(track)                
                    miniHistsPlus_qq[5].fillTrack(track)                
                if track.charge() == -1: 
                    miniHistsMinus[5].fillTrack(track)                
                    miniHistsMinus_gg[5].fillTrack(track)                
                    miniHistsMinus_qg[5].fillTrack(track)                
                    miniHistsMinus_qq[5].fillTrack(track)                
                miniHistsSum[5].fillTrack(track)
                miniHistsSum_gg[5].fillTrack(track)
                miniHistsSum_qg[5].fillTrack(track)
                miniHistsSum_qq[5].fillTrack(track)
                
                if trigDidFire[0]: 
                    if track.charge() == 1:  
                        miniHistsPlus[0].fillTrack(track)                
                        miniHistsPlus_gg[0].fillTrack(track)                
                        miniHistsPlus_qg[0].fillTrack(track)                
                        miniHistsPlus_qq[0].fillTrack(track)                
                    if track.charge() == -1: 
                        miniHistsMinus[0].fillTrack(track)                
                        miniHistsMinus_gg[0].fillTrack(track)                
                        miniHistsMinus_qg[0].fillTrack(track)                
                        miniHistsMinus_qq[0].fillTrack(track)                
                    miniHistsSum[0].fillTrack(track)
                    miniHistsSum_gg[0].fillTrack(track)
                    miniHistsSum_qg[0].fillTrack(track)
                    miniHistsSum_qq[0].fillTrack(track)
                    for i in range(4):
                        if trigDidFire[i+1]: 
                            if track.charge() == 1:  
                                miniHistsPlus[i+1].fillTrack(track)
                                miniHistsPlus_gg[i+1].fillTrack(track)
                                miniHistsPlus_qg[i+1].fillTrack(track)
                                miniHistsPlus_qq[i+1].fillTrack(track)
                            if track.charge() == -1: 
                                miniHistsMinus[i+1].fillTrack(track)
                                miniHistsMinus_gg[i+1].fillTrack(track)
                                miniHistsMinus_qg[i+1].fillTrack(track)
                                miniHistsMinus_qq[i+1].fillTrack(track)
                            miniHistsSum[i+1].fillTrack(track)
                            miniHistsSum_gg[i+1].fillTrack(track)
                            miniHistsSum_qg[i+1].fillTrack(track)
                            miniHistsSum_qq[i+1].fillTrack(track)
            
        skimFile.Close()
        miniFile.Close()
    
    #miniHists[0].draw()
    #miniHists[-1].draw()
    
    outFilePlus.cd()
    eventCounter.Write()
    for m in miniHistsPlus: [h.Write() for h in m.values()]
    for m in miniHistsPlus_gg: [h.Write() for h in m.values()]
    for m in miniHistsPlus_qg: [h.Write() for h in m.values()]
    for m in miniHistsPlus_qq: [h.Write() for h in m.values()]
    outFilePlus.Close()
    
    outFileMinus.cd()
    eventCounter.Write()
    for m in miniHistsMinus: [h.Write() for h in m.values()]
    for m in miniHistsMinus_gg: [h.Write() for h in m.values()]
    for m in miniHistsMinus_qg: [h.Write() for h in m.values()]
    for m in miniHistsMinus_qq: [h.Write() for h in m.values()]
    outFileMinus.Close()
    
    outFileSum.cd()
    eventCounter.Write()
    for m in miniHistsSum: [h.Write() for h in m.values()]
    for m in miniHistsSum_gg: [h.Write() for h in m.values()]
    for m in miniHistsSum_qg: [h.Write() for h in m.values()]
    for m in miniHistsSum_qq: [h.Write() for h in m.values()]
    outFileSum.Close()


def calculateALL(num, denom, nevents, sample_weights):
    #error calculations in this function follow the recipe derived by Jim Sowinski
    
    #don't include a bin from an individual sample in content or error if it has fewer than this # of particles
    minParticlesToAccept = 10
    
    #rebinning
    #[h.Rebin(2) for h in num]
    #[h.Rebin(2) for h in denom]
    
    top = []
    bot = []
    content = []
    avg_asym_weight = []
    nparticles = []
    
    [avg_asym_weight.append([]) for sample in num]
    [nparticles.append([]) for sample in num]
    
    for bin in range(num[0].GetNbinsX()):
        top.append(0.)
        bot.append(0.)
        content.append(0.)
        
        for sample in range(len(num)):
            nparticles[sample].append(denom[sample].GetBinContent(bin+1))
            if nparticles[sample][bin] >= minParticlesToAccept:
                avg_asym_weight[sample].append(num[sample].GetBinContent(bin+1) / nparticles[sample][bin])
                top[bin] += num[sample].GetBinContent(bin+1) * sample_weights[sample] / nevents[sample]
                bot[bin] += denom[sample].GetBinContent(bin+1) * sample_weights[sample] / nevents[sample]
            else:
                avg_asym_weight[sample].append(0.)
        
        if bot[bin] != 0.: content[bin] = top[bin] / bot[bin]
    
    #calculating the errors requires another loop over all samples
    error = []
    for bin in range(num[0].GetNbinsX()):
        error.append(0.)
        
        for sample in range(len(num)):
            if nparticles[sample][bin] >= minParticlesToAccept:
                first_term = avg_asym_weight[sample][bin] - content[bin]
                second_term = num[sample].GetBinError(bin+1) * num[sample].GetBinError(bin+1) / nparticles[sample][bin] - avg_asym_weight[sample][bin] * avg_asym_weight[sample][bin]
            else:
                first_term = 0
                second_term = 0
            wp = sample_weights[sample] / nevents[sample]
            
            error[bin] += wp*wp*nparticles[sample][bin] * (first_term*first_term*(1+nparticles[sample][bin]/nevents[sample]) + second_term)
            
        if bot[bin] != 0.: error[bin] *= 1.0 / (bot[bin] * bot[bin])
    
    h = num[0].Clone()
    
    #update the new histo
    for bin in range(num[0].GetNbinsX()):
        if bot[bin] == 0.:
            content[bin] = 0.
            error[bin] = 0.
        
        h.SetBinContent(bin+1, content[bin])
        h.SetBinError(bin+1, math.sqrt(error[bin]))
        
    return h


def combine(outFilePath, inputFileList, useWeights=False): 
    """combines histograms in arg2 into a file named arg1"""
    minCounts = 5.5 ## don't take gg events with 1 real count
    
    subProcessWeights={'gg':1.0, 'qg':1.0, 'qq':1.0}
    
    outFile = ROOT.TFile(outFilePath,'recreate')
    
    for subProcess in ('','gg','qg','qq'):
        print 'now combining subProcess =',subProcess
        outHists = [ MiniMcHistos(key, subProcess) for key in miniKeys ]
        
        #print outHists
        #keep all files open to do the asymmetry calculation
        tfile = [ ROOT.TFile(path,'read') for path in inputFileList ]
        nevents = [ f.Get('eventCounter').GetBinContent(1) for f in tfile ]
        xsec_weight = [ xsec[os.path.basename(path).split('.')[0]] for path in inputFileList ]
        
        sampleHists = [] 
        
        for index in range(len(inputFileList)):
            print 'adding', inputFileList[index]
        
            sampleHists.append( [ MiniMcHistos(key, subProcess, tfile[index]) for key in miniKeys ] )
            
            # reweight subProcesses?
            if useWeights and subProcess == '':
                m = sampleHists[-1]
                gg = [ MiniMcHistos(key, 'gg', tfile[index]) for key in miniKeys ]
                qg = [ MiniMcHistos(key, 'qg', tfile[index]) for key in miniKeys ]
                qq = [ MiniMcHistos(key, 'qq', tfile[index]) for key in miniKeys ]
                for i in range(len(outHists)):
                    for key in m[i].keys():
                        m[i][key].Reset()
                        m[i][key].Add(gg[i][key], subProcessWeights['gg'])
                        m[i][key].Add(qg[i][key], subProcessWeights['qg'])
                        m[i][key].Add(qq[i][key], subProcessWeights['qq'])
            
            for i in range(len(outHists)):
                outHists[i].add(sampleHists[-1][i],xsec_weight[index],nevents[index],minCounts)
        
        #now calculate A_LL    
        for trig in range(len(sampleHists[0])):
            for key in asymKeys:
                num = [sample[trig][key] for sample in sampleHists]
                denom = [sample[trig]['denom'] for sample in sampleHists]
                outHists[trig][key].Delete()
                outHists[trig][key] = calculateALL(num, denom, nevents, xsec_weight)
        
        outFile.cd()
        for m in outHists:
            [h.Write() for h in m.values()]    
            
        [f.Close() for f in tfile]
        
    #outFile = ROOT.TFile(outFilePath,'recreate')
    #print 'write', outFilePath
    #for m in keepMe: [h.Write() for h in m.values()]
    
    


def partonicCrossSection(sample='2_3', nevents=1000, sqrts=200):
    """runs standalone Pythia to determine xsec for weighting purposes, returns xsec"""
    import ROOT
    pythia = ROOT.TPythia6()
    
    ckMin = sample.split('_')[0]
    if ckMin == 'above': 
        ckMin = 35
    else:
        ckMin = float(ckMin)
    
    ckMax = sample.split('_')[1]
    if ckMax == '35':
        ckMax = 1000
    else:
        ckMax = float(ckMax)
    
    #pythia.SetMSTP(51, 8)
    #pythia.SetMSEL(1)
    
    # CDF Tune A for STAR
    pythia.SetMSEL(1) # could also be 2, but gives VERY different results
    pythia.SetMSTP(51, 7)
    pythia.SetMSTP(81, 1)
    pythia.SetMSTP(82, 4)
    pythia.SetPARP(67, 4.0)
    pythia.SetPARP(83, 0.5)
    pythia.SetPARP(84, 0.4)
    pythia.SetPARP(85, 0.9)
    pythia.SetPARP(86, 0.95)
    pythia.SetPARP(89, 1800)
    pythia.SetPARP(90, 0.25)
    pythia.SetPARP(91, 1.0)
    
    pythia.SetCKIN(3, ckMin)
    pythia.SetCKIN(4, ckMax)
    
    pythia.Initialize('CMS', 'p', 'p', sqrts)
    
    for i in range(nevents):
        if i % 1000 == 0: print 'generating event', i
        pythia.GenerateEvent()
        
    pythia.Pystat(1)
    
    return pythia.GetPARI(1)


## the rest of the functions just make plots

def ptResolution(histFile='./combined.sum.hist.root', trigKey='notrig', subProcess=''):
    #import mystyle; mystyle.use(1)
    f = ROOT.TFile(histFile)
    m = MiniMcHistos(trigKey, subProcess, f)
    
    c = ROOT.TCanvas()
    m['ptMc_ptPr'].GetZaxis().SetRangeUser(1e-8,20)
    m['ptMc_ptPr'].Draw()
    
    ROOT.gPad.SetLogz()
    
    raw_input('beli')
    return(f,m,c)


def triggerBias(histFile='./combined.plus.hist.root', trigKey='96233', scenario='nlo'):
    tfile = ROOT.TFile(histFile)
    
    notrig = MiniMcHistos('notrig', '', tfile)
    m = MiniMcHistos(trigKey, '', tfile)
    
    c = ROOT.TCanvas('c')
    notrig[scenario].GetXaxis().SetRangeUser(1,11)
    notrig[scenario].GetYaxis().SetRangeUser(-0.08,0.08)
    if 'plus' in histFile:
        notrig[scenario].SetMarkerStyle(20)
        m[scenario].SetMarkerStyle(24)
        charge = 'plus'
    if 'minus' in histFile:
        notrig[scenario].SetMarkerStyle(21)
        m[scenario].SetMarkerStyle(25)
        charge = 'minus'
    notrig[scenario].SetTitle('%s - %s - %s' % (charge, trigKey, scenario))
        
    notrig[scenario].Draw()
    m[scenario].Draw('same')
    #m[scenario].Rebin()
    
    #draw a graph representing size of systematic
    nbins = m[scenario].GetNbinsX()
    sys = ROOT.TGraphAsymmErrors(nbins)
    for bin in range(nbins+1):
        x = m[scenario].GetBinCenter(bin)
        y = -0.07
        diff = m[scenario].GetBinContent(bin) - notrig[scenario].GetBinContent(bin)
        stat = m[scenario].GetBinError(bin)
        #print x, diff, stat
        sys.SetPoint(bin-1, x, y)
        sys.SetPointError( bin-1 , 0, 0, 0, max(diff,stat) )
    #sys.SetPointError(0, 0, 0, 0, 0.000)
    sys.Draw('f same')
    
    raw_input('blerg?')
    tfile.Close()
    
    return sys


def triggerBiasAllScenarios(histFile='./combined.plus.hist.root', trigKey='96233'):
    asymKeys2.reverse()
    graphs = [triggerBias(histFile,trigKey,scenario) for scenario in asymKeys2]
    
    yoffsets = [ 0.05*i for i in range(len(asymKeys2)) ]
    
    # stupid stupid stupid
    xbins = [0.5*i+0.25 for i in range(graphs[0].GetN())]
    
    c = ROOT.TCanvas('c2')
    bg = ROOT.TH2D('bg','',1,1,11,1,-0.05,yoffsets[-1]+0.1)
    bg.Draw()
    
    for row,graph in enumerate(graphs):
        for i in range(graph.GetN()):
            #graph.SetPoint(i, graph.GetXaxis().GetBinCenter(i+1), yoffsets[row] )
            graph.SetPoint(i, xbins[i], yoffsets[row] )
        graph.Draw('f same')
        
    # now find max value for each bin
    maxValues = [ max([gr.GetErrorYhigh(i) for gr in graphs]) for i in range(graphs[0].GetN()) ]
    #print maxValues
    
    c2 = ROOT.TCanvas('c3')
    maxGraph = ROOT.TGraphAsymmErrors(len(maxValues))
    for i,val in enumerate(maxValues):
        maxGraph.SetPoint(i, xbins[i], 0)
        maxGraph.SetPointEYhigh(i, maxValues[i])
    maxGraph.GetXaxis().SetRangeUser(1,11)
    maxGraph.Draw('a f')
    
    raw_input('big blerg')


def subProcessSpectra(histFile='./combined.sum.hist.root', trigKey='notrig'):
    tfile = ROOT.TFile(histFile)
    
    bins = (2,3,4,5,7,9,11,15,20,25,30,35,70)
    
    m_gg = MiniMcHistos(trigKey, 'gg', tfile)
    m_qg = MiniMcHistos(trigKey, 'qg', tfile)
    m_qq = MiniMcHistos(trigKey, 'qq', tfile)
    
    ps = ROOT.TPostScript('subprocess_spectra.ps')
    c = ROOT.TCanvas('c','',100,100,600,800)
    c.Divide(3,4)
    ps.NewPage()
    for i in range(len(bins)-1):
        
        c.cd(i+1)
        ROOT.gPad.SetLogy()
        
        h_gg = m_gg['partonicPt'].ProjectionX('%d_gg_px' % (i,), bins[i]+1, bins[i+1]+1)
        h_gg.SetTitle('%d < partonic p_{T} < %d' % (bins[i], bins[i+1]))
        h_gg.SetXTitle('#pi reco p_{T}')
        h_gg.SetMarkerStyle(20)
        h_gg.SetMarkerColor(ROOT.kRed)
        h_gg.DrawCopy()
        
        h_qg = m_qg['partonicPt'].ProjectionX('%d_gg_px' % (i,), bins[i]+1, bins[i+1]+1)
        h_gg.SetMarkerStyle(25)
        h_gg.SetMarkerColor(ROOT.kBlue)
        h_qg.DrawCopy('same')
        
        h_qq = m_qq['partonicPt'].ProjectionX('%d_gg_px' % (i,), bins[i]+1, bins[i+1]+1)
        h_gg.SetMarkerStyle(21)
        h_gg.SetMarkerColor(ROOT.kGreen)
        h_qq.DrawCopy('same')
        
    c.Update()
    raw_input('pause')
    ps.Close()


def subProcessPionMultiplicity(histFile='./combined.sum.hist.root', trigKey='notrig'):
    proj_gg = whatDoICallThis(histFile, trigKey, 'gg')
    proj_qg = whatDoICallThis(histFile, trigKey, 'qg')
    proj_qq = whatDoICallThis(histFile, trigKey, 'qq')
    
    bg = ROOT.TH2D('bg','',1,0,35,1,2,12)
    bg.SetXTitle('event partonic p_{T}')
    bg.SetYTitle('< nChargedPions >')
    
    proj_gg[-1].SetMarkerStyle(20)
    proj_qg[-1].SetMarkerStyle(25)
    proj_qq[-1].SetMarkerStyle(21)
    
    proj_gg[-1].SetMarkerColor(ROOT.kRed)
    proj_qg[-1].SetMarkerColor(ROOT.kBlue)
    proj_qq[-1].SetMarkerColor(ROOT.kGreen)
    
    leg = ROOT.TLegend(0.15,0.65,0.3,0.85)
    leg.AddEntry(proj_gg[-1],'gg','p')
    leg.AddEntry(proj_qg[-1],'qg','p')
    leg.AddEntry(proj_qq[-1],'qq','p')
    
    c = ROOT.TCanvas()
    bg.Draw()
    proj_gg[-1].Draw('same')
    proj_qg[-1].Draw('same')
    proj_qq[-1].Draw('same')
    leg.Draw()
    
    raw_input('stop here for instructions')
    
    c.Print('subprocess_pion_multiplicity.gif')
    


def meanPtVersusPartonicPtAllSub(histFile='./combined.sum.hist.root', trigKey='notrig'):
    #import mystyle; mystyle.use(1)
    ROOT.gStyle.SetOptStat(0)
    
    proj_gg = meanPtVersusPartonicPt(histFile, trigKey, 'gg')
    proj_qg = meanPtVersusPartonicPt(histFile, trigKey, 'qg')
    proj_qq = meanPtVersusPartonicPt(histFile, trigKey, 'qq')
    
    bg = ROOT.TH2D('bg','',1,0,35,1,0.4,2.3)
    bg.SetXTitle('event partonic p_{T}')
    bg.SetYTitle('< #pi p_{T} >')
    
    proj_gg[-1].SetMarkerStyle(20)
    proj_qg[-1].SetMarkerStyle(25)
    proj_qq[-1].SetMarkerStyle(21)
    
    proj_gg[-1].SetMarkerColor(ROOT.kRed)
    proj_qg[-1].SetMarkerColor(ROOT.kBlue)
    proj_qq[-1].SetMarkerColor(ROOT.kGreen)
    
    leg = ROOT.TLegend(0.15,0.65,0.3,0.85)
    leg.AddEntry(proj_gg[-1],'gg','p')
    leg.AddEntry(proj_qg[-1],'qg','p')
    leg.AddEntry(proj_qq[-1],'qq','p')
    
    c = ROOT.TCanvas()
    bg.Draw()
    proj_gg[-1].Draw('same')
    proj_qg[-1].Draw('same')
    proj_qq[-1].Draw('same')
    leg.Draw()
    
    raw_input('pause:')
    
    c.Print('mean_pt_subprocess.gif')


def meanPtVersusPartonicPt(histFile='./combined.sum.hist.root', trigKey='notrig', subProcess=''):
    """note:  errors are absolutely fake"""
    f = ROOT.TFile(histFile)
    m = MiniMcHistos(trigKey, subProcess, f)
    
    c = ROOT.TCanvas()
    m['partonicPt'].GetZaxis().SetRangeUser(m['partonicPt'].GetMinimum(1e-10), m['partonicPt'].GetMaximum())
    m['partonicPt'].Draw('col')
    
    ROOT.gPad.SetLogz()
    
    c2 = ROOT.TCanvas()
    
    proj = m['partonicPt'].ProfileY('meanPt'+histFile+trigKey+subProcess)
    proj = proj.ProjectionX()
    [proj.SetBinError(b+1, 0.01) for b in range(proj.GetNbinsX())]
    
    proj.GetXaxis().SetRangeUser(2,45)
    proj.SetYTitle('< #pi p_{T} >')
    proj.SetXTitle('event partonic p_{T}')
    proj.SetTitle('Subprocess (if any): ' + subProcess)
    proj.Draw('e')
    return [f,m,c,c2,proj]


def whatDoICallThis(histFile='./combined.sum.hist.root', trigKey='notrig', subProcess=''):
    """plots <nChargedPions> ov. partonic pT"""
    f = ROOT.TFile(histFile)
    m = MiniMcHistos(trigKey, subProcess, f)
    
    c = ROOT.TCanvas()
    m['partonicPt'].GetZaxis().SetRangeUser(m['partonicPt'].GetMinimum(1e-10), m['partonicPt'].GetMaximum())
    m['partonicPt'].Draw('col')
    
    ROOT.gPad.SetLogz()
    
    c2 = ROOT.TCanvas()
    proj = m['partonicPt'].ProjectionY('whatDo'+histFile+trigKey+subProcess)
    proj.Divide( m['eventPartonicPt'].Rebin(20) )
    
    proj.GetXaxis().SetRangeUser(2,45)
    proj.SetYTitle('<nChargedPions>')
    proj.SetXTitle('event partonic p_{T}')
    proj.SetTitle('Subprocess (if any): ' + subProcess)
    proj.Draw('e')
    return [f,m,c,c2,proj]


def pionMultiplicity(histFile='./7_9.plus.hist.root', trigKey='notrig'):
    """this is probably junk"""
    f = ROOT.TFile(histFile)
    m = MiniMcHistos(trigKey, f)
    
    m['gg'].Scale( 1.0 / (m['eventPartonicPt_gg'].GetEntries()) )
    m['qg'].Scale( 1.0 / (m['eventPartonicPt_qg'].GetEntries()) )
    m['qq'].Scale( 1.0 / (m['eventPartonicPt_qq'].GetEntries()) )
    
    for key in ['gg','qg','qq']:
        [ m[key].SetBinError(b+1,0.1*m[key].GetBinContent(b+1)) for b in range(m[key].GetNbinsX()) ]
    
    m['gg'].SetMarkerColor(ROOT.kRed)
    m['qg'].SetMarkerColor(ROOT.kBlue)
    m['qq'].SetMarkerColor(ROOT.kGreen)
    
    m['gg'].SetMarkerStyle(20)
    m['qg'].SetMarkerStyle(25)
    m['qq'].SetMarkerStyle(21)
    
    c1 = ROOT.TCanvas()
    
    m['gg'].Draw('p')
    m['qg'].Draw('p same')
    m['qq'].Draw('p same')
    
    ROOT.gPad.SetLogy()
    raw_input('what do you think?')


def subProcessShiftFromTrigger(histFile='/Users/kocolosk/data/simu/combined.hist.root'):
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetPadGridY(True)
    
    tfile = ROOT.TFile(histFile,'read')
    
    minimc = [ MiniMcHistos(key, tfile) for key in miniKeys ]
    
    #rescale to get bin-by-bin fractions
    for i in range(minimc[0].ptBins[0]):
        for j in range(len(minimc)):
            a = minimc[j]['gg'].GetBinContent(i+1)
            b = minimc[j]['qg'].GetBinContent(i+1)
            c = minimc[j]['qq'].GetBinContent(i+1)
            
            ea = minimc[j]['gg'].GetBinError(i+1)
            eb = minimc[j]['qg'].GetBinError(i+1)
            ec = minimc[j]['qq'].GetBinError(i+1)
            
            binSum = a + b + c
            if binSum > 0:
                minimc[j]['gg'].SetBinContent(i+1,a/binSum)
                minimc[j]['qg'].SetBinContent(i+1,b/binSum)
                minimc[j]['qq'].SetBinContent(i+1,c/binSum)
                
                minimc[j]['gg'].SetBinError(i+1,ea/binSum)
                minimc[j]['qg'].SetBinError(i+1,eb/binSum)
                minimc[j]['qq'].SetBinError(i+1,ec/binSum)
    
    #fun with graphics
    [m['gg'].SetLineColor(ROOT.kRed) for m in minimc]
    [m['qg'].SetLineColor(ROOT.kBlue) for m in minimc]
    [m['qq'].SetLineColor(ROOT.kGreen) for m in minimc]
    
    [m['gg'].SetMarkerColor(ROOT.kRed) for m in minimc]
    [m['qg'].SetMarkerColor(ROOT.kBlue) for m in minimc]
    [m['qq'].SetMarkerColor(ROOT.kGreen) for m in minimc]
    
    [m['gg'].SetMarkerSize(0.6) for m in minimc]
    [m['qg'].SetMarkerSize(0.6) for m in minimc]
    [m['qq'].SetMarkerSize(0.6) for m in minimc]
    
    [m['gg'].SetMarkerStyle(20) for m in minimc]
    [m['qg'].SetMarkerStyle(20) for m in minimc]
    [m['qq'].SetMarkerStyle(20) for m in minimc]
    
    [m['gg'].SetLineStyle(2) for m in minimc[1:]]
    [m['qg'].SetLineStyle(2) for m in minimc[1:]]
    [m['qq'].SetLineStyle(2) for m in minimc[1:]]
    
    leg = ROOT.TLegend(0.65,0.7,0.88,0.88,'solid=MB, dash=trigger')
    leg.AddEntry(minimc[0]['gg'],'gg','l')
    leg.AddEntry(minimc[0]['qg'],'qg','l')
    leg.AddEntry(minimc[0]['qq'],'qq','l')
    
    minimc[1]['gg'].SetTitle('HT1')
    minimc[2]['gg'].SetTitle('HT2')
    minimc[3]['gg'].SetTitle('JP1')
    minimc[4]['gg'].SetTitle('JP2')
    
    bg = ROOT.TH2D('bg','',1,0,15,1,0,0.9)
    bg.SetXTitle('#pi reco p_{T}')
    
    c = [ROOT.TCanvas('c_ht1'), ROOT.TCanvas('c_ht2'), ROOT.TCanvas('c_jp1'), ROOT.TCanvas('c_jp2')]
    for i in range(4):
        c[i].cd()
        bg.SetTitle(minimc[i+1]['gg'].GetTitle())
        bg.DrawCopy()
        minimc[0]['gg'].Draw('e1 same')
        minimc[0]['qg'].Draw('e1 same')
        minimc[0]['qq'].Draw('e1 same')
        
        minimc[i+1]['gg'].Draw('e1 same')
        minimc[i+1]['qg'].Draw('e1 same')
        minimc[i+1]['qq'].Draw('e1 same')
        leg.Draw()
    raw_input('press enter to continue:')


def subProcessContributions(histFile='./combined.minus.hist.root', trigKey='notrig'):
    tfile = ROOT.TFile(histFile,'read')
    
    minimc = MiniMcHistos(trigKey, '', tfile)
    minimc_gg = MiniMcHistos(trigKey, 'gg', tfile)
    minimc_qg = MiniMcHistos(trigKey, 'qg', tfile)
    minimc_qq = MiniMcHistos(trigKey, 'qq', tfile)
    
    #rescale to get bin-by-bin fractions
    for i in range(minimc.ptBins[0]):
        a = minimc_gg['pt'].GetBinContent(i+1)
        b = minimc_qg['pt'].GetBinContent(i+1)
        c = minimc_qq['pt'].GetBinContent(i+1)
        
        ea = minimc_gg['pt'].GetBinError(i+1)
        eb = minimc_qg['pt'].GetBinError(i+1)
        ec = minimc_qq['pt'].GetBinError(i+1)
        
        binSum = a + b + c
        if binSum > 0:
            minimc_gg['pt'].SetBinContent(i+1,a/binSum)
            minimc_qg['pt'].SetBinContent(i+1,b/binSum)
            minimc_qq['pt'].SetBinContent(i+1,c/binSum)
            
            minimc_gg['pt'].SetBinError(i+1,ea/binSum)
            minimc_qg['pt'].SetBinError(i+1,eb/binSum)
            minimc_qq['pt'].SetBinError(i+1,ec/binSum)
    
    minimc_gg['pt'].SetMarkerStyle(20)
    minimc_qg['pt'].SetMarkerStyle(25)
    minimc_qq['pt'].SetMarkerStyle(21)
    
    bg = ROOT.TH2D('bg','',1,0,15,1,0,0.9)
    bg.SetXTitle('#pi reco p_{T}')
    bg.SetTitle('Subprocess Contributions for #pi^{+} Production')
    
    c1 = ROOT.TCanvas('c1')
    bg.Draw()
    minimc_gg['pt'].Draw('e1 same')
    minimc_qg['pt'].Draw('e1 same')
    minimc_qq['pt'].Draw('e1 same')
    
    leg = ROOT.TLegend(0.75,0.7,0.88,0.88)
    leg.AddEntry(minimc_gg['pt'],'gg','p')
    leg.AddEntry(minimc_qg['pt'],'qg','p')
    leg.AddEntry(minimc_qq['pt'],'qq','p')
    leg.Draw()
    
    raw_input('What do you think?')
    #c1.Print('subprocess_contributions_minus.gif')


def subProcessContributionsEvent(histFile='./combined.minus.hist.root', trigKey='notrig'):
    tfile = ROOT.TFile(histFile,'read')
    
    minimc = MiniMcHistos(trigKey, '', tfile)
    mgg = MiniMcHistos(trigKey, 'gg', tfile)
    mqg = MiniMcHistos(trigKey, 'qg', tfile)
    mqq = MiniMcHistos(trigKey, 'qq', tfile)
    
    #rescale to get bin-by-bin fractions
    for i in range(minimc.eventPartonBins[0]):
        a = mgg['eventPartonicPt'].GetBinContent(i+1)
        b = mqg['eventPartonicPt'].GetBinContent(i+1)
        c = mqq['eventPartonicPt'].GetBinContent(i+1)
        
        ea = mgg['eventPartonicPt'].GetBinError(i+1)
        eb = mqg['eventPartonicPt'].GetBinError(i+1)
        ec = mqq['eventPartonicPt'].GetBinError(i+1)
        
        binSum = a + b + c
        if binSum > 0:
            mgg['eventPartonicPt'].SetBinContent(i+1,a/binSum)
            mqg['eventPartonicPt'].SetBinContent(i+1,b/binSum)
            mqq['eventPartonicPt'].SetBinContent(i+1,c/binSum)
            
            mgg['eventPartonicPt'].SetBinError(i+1,ea/binSum)
            mqg['eventPartonicPt'].SetBinError(i+1,eb/binSum)
            mqq['eventPartonicPt'].SetBinError(i+1,ec/binSum)
    
    mgg['eventPartonicPt'].SetMarkerStyle(20)
    mqg['eventPartonicPt'].SetMarkerStyle(25)
    mqq['eventPartonicPt'].SetMarkerStyle(21)
    
    mgg['eventPartonicPt'].SetMarkerColor(ROOT.kRed)
    mqg['eventPartonicPt'].SetMarkerColor(ROOT.kBlue)
    mqq['eventPartonicPt'].SetMarkerColor(ROOT.kGreen)
    
    bg = ROOT.TH2D('bg','',1,2,30,1,0,0.9)
    bg.SetXTitle('event partonic p_{T}')
    bg.SetTitle('Subprocess Mixture')
    
    c1 = ROOT.TCanvas('c1')
    bg.Draw()
    mgg['eventPartonicPt'].Draw('e1 same')
    mqg['eventPartonicPt'].Draw('e1 same')
    mqq['eventPartonicPt'].Draw('e1 same')
    
    leg = ROOT.TLegend(0.75,0.7,0.88,0.88)
    leg.AddEntry(mgg['eventPartonicPt'],'gg','p')
    leg.AddEntry(mqg['eventPartonicPt'],'qg','p')
    leg.AddEntry(mqq['eventPartonicPt'],'qq','p')
    leg.Draw()
    
    raw_input('What do you think?')


def partonicPt(histFile='/Users/kocolosk/data/simu/combined.hist.root'):
    ROOT.gStyle.SetOptLogy(False)
    ROOT.gStyle.SetOptLogz(False)
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetPadGridX(False)
    ROOT.gStyle.SetPadGridY(True)
    
    tfile = ROOT.TFile(histFile,'read')
    
    minimc = [ MiniMcHistos(key, tfile) for key in miniKeys ]
    
    h = [   ROOT.TH1D('tmp_minbias','',minimc[0].ptBins[0],minimc[0].ptBins[1],minimc[0].ptBins[2]), 
            ROOT.TH1D('tmp_96201','',minimc[0].ptBins[0],minimc[0].ptBins[1],minimc[0].ptBins[2]), 
            ROOT.TH1D('tmp_96211','',minimc[0].ptBins[0],minimc[0].ptBins[1],minimc[0].ptBins[2]), 
            ROOT.TH1D('tmp_96221','',minimc[0].ptBins[0],minimc[0].ptBins[1],minimc[0].ptBins[2]), 
            ROOT.TH1D('tmp_96233','',minimc[0].ptBins[0],minimc[0].ptBins[1],minimc[0].ptBins[2]), ]
    
    c = ROOT.TCanvas()
    
    for i in range(len(minimc)):
        for j in range(minimc[i].ptBins[0]):
            minimc[i]['partonicPt'].GetXaxis().SetRange(j+1,j+1)
            h[i].SetBinContent(j+1, h[i].GetBinCenter(j+1) / minimc[i]['partonicPt'].GetMean(2))
            h[i].SetBinError(j+1, minimc[i]['partonicPt'].GetMeanError(2) * h[i].GetBinContent(j+1) / minimc[i]['partonicPt'].GetMean(2))
    
    h[0].SetMarkerStyle(2)
    h[1].SetMarkerStyle(24)
    h[2].SetMarkerStyle(20)
    h[3].SetMarkerStyle(25)
    h[4].SetMarkerStyle(21)
    
    #bg = ROOT.TH2D('bg','',1,0,15,1,0,30)
    bg = ROOT.TH2D('bg','',1,0,15,1,0,0.8)
    bg.SetTitle('Fraction of parton p_{T} carried by #pi^{+/-}')
    bg.SetXTitle('#pi reco p_{T}')
    bg.SetYTitle('#pi reco p_{T} / < partonic p_{T} >')
    #bg.SetYTitle('< partonic p_{T} >')
    bg.DrawCopy()
    [tmp.Draw('same') for tmp in h]
    
    leg = ROOT.TLegend(0.65,0.15,0.89,0.45)
    leg.AddEntry(h[0],'minbias','p')
    leg.AddEntry(h[1],'ht1','p')
    leg.AddEntry(h[2],'ht2','p')
    leg.AddEntry(h[3],'jp1','p')
    leg.AddEntry(h[4],'jp2','p')
    
    leg.Draw('same')
    
    raw_input('press enter to continue:')


def cfactor(histFile='/Users/kocolosk/data/simu/combined.hist.root'):
    tfile = ROOT.TFile(histFile,'read')
    
    minimc = {}
    for key in miniKeys: minimc[key] = MiniMcHistos(key, tfile)
    
    ROOT.gStyle.SetOptLogy(True)
    c = ROOT.TCanvas()
    
    h = [ minimc['minbias']['pt'],
          minimc['96201']['pt'], 
          minimc['96211']['pt'],
          minimc['96221']['pt'],
          minimc['96233']['pt'] ]
          
    [ elem.Divide(minimc['notrig']['pt']) for elem in h ]
    
    h[0].SetXTitle('p_{T}')
    h[0].SetYTitle('trigger tracks / total tracks')
    
    h[0].SetMarkerStyle(2)
    h[1].SetMarkerStyle(24)
    h[2].SetMarkerStyle(20)
    h[3].SetMarkerStyle(25)
    h[4].SetMarkerStyle(21)
    
    h[0].Draw()
    [ elem.Draw('same') for elem in h[1:] ]
    
    leg = ROOT.TLegend(0.65,0.15,0.89,0.45)
    leg.AddEntry(h[0],'minbias','p')
    leg.AddEntry(h[1],'ht1','p')
    leg.AddEntry(h[2],'ht2','p')
    leg.AddEntry(h[3],'jp1','p')
    leg.AddEntry(h[4],'jp2','p')
    
    leg.Draw('same')
    
    raw_input('press enter to continue:')
    


def asymmetries(histFile='/Users/kocolosk/data/simu/combined.hist.root'):
    """plots all triggers for an asmmyetry on a single canvas"""
    tfile = ROOT.TFile(histFile,'read')
    
    minimc = [ MiniMcHistos(key, '', tfile) for key in miniKeys ]
                    
    c = [ ROOT.TCanvas('c0'), ROOT.TCanvas('c1'), ROOT.TCanvas('c2'), ROOT.TCanvas('c3'), ROOT.TCanvas('c4') ]
    
    keys = ['lo', 'nlo', 'max', 'min', 'zero']
    
    [ minimc[0][key].GetXaxis().SetRangeUser(2.0, 10.0) for key in keys ]
    
    [ minimc[0][key].SetLineColor(ROOT.kRed) for key in keys ]
    [ minimc[1][key].SetLineColor(ROOT.kGreen) for key in keys ]
    [ minimc[2][key].SetLineColor(ROOT.kGreen) for key in keys ]
    [ minimc[3][key].SetLineColor(ROOT.kBlue) for key in keys ]
    [ minimc[4][key].SetLineColor(ROOT.kBlue) for key in keys ]
    
    
    c[0].cd()
    minimc[0]['nlo'].Draw()
    minimc[2]['nlo'].Draw('same')
    minimc[4]['nlo'].Draw('same')
    
    c[1].cd()
    minimc[0]['max'].Draw()
    minimc[2]['max'].Draw('same')
    minimc[4]['max'].Draw('same')
    
    c[2].cd()
    minimc[0]['min'].Draw()
    minimc[2]['min'].Draw('same')
    minimc[4]['min'].Draw('same')
    
    c[3].cd()
    minimc[0]['zero'].Draw()
    minimc[2]['zero'].Draw('same')
    minimc[4]['zero'].Draw('same')
    
    c[4].cd()
    minimc[0]['nlo'].Draw()
    minimc[1]['nlo'].Draw('same')
    minimc[3]['nlo'].Draw('same')
    
    raw_input('press enter to continue:')


def asymmetries2(histFile='./combined.plus.hist.root', trigKey='notrig'):
    """plots all asymmetries for trig on one canvas"""
    f = ROOT.TFile(histFile)
    minimc = MiniMcHistos(trigKey, '', f)
    
    c1 = ROOT.TCanvas('c1')
    bg = ROOT.TH2D('bg','Raw #pi^{-} Asymmetries for All Scenarios ',1,1.0,15.0,1,-0.04,0.08)
    bg.SetXTitle('#pi reco p_{T}')
    bg.Draw()
    
    leg = ROOT.TLegend(0.78,0.11,0.89,0.89)
    
    minimc['zero'].SetMarkerColor(ROOT.kBlue)
    minimc['min'].SetMarkerColor(ROOT.kGreen)
    minimc['max'].SetMarkerColor(ROOT.kRed)
    minimc['nlo'].SetMarkerColor(7)
    
    # excluding p070 b/c of a bug on my part (oops)
    for row,key in enumerate(asymKeys[1:-1]):
        if row>3: 
            minimc[key].SetMarkerStyle(row+16)
        else:
            minimc[key].SetMarkerStyle(20)
        
        # reset errors for clarity
        [ minimc[key].SetBinError(b+1,0) for b in range(minimc[key].GetNbinsX()) ]
        
        minimc[key].GetXaxis().SetRangeUser(1.0, 11.0)
        minimc[key].Draw('p same x0')
        leg.AddEntry(minimc[key], key, 'p')
        
    leg.Draw()
    
    raw_input('what do you think?')
    #c1.Print('all_scenarios_minus.gif')


def asymmetries3(histFile='./combined.plus.hist.root', trigger='96233', scenario='nlo', canvas=None):
    """plots deviation from raw A_{LL} v. p_T"""
    f = ROOT.TFile(histFile)
    m = {}
    for key in trigKeys: m[key] = MiniMcHistos(key, '', f)
    
    if 'plus' in histFile: charge = '+'
    else: charge = '-'
    
    if canvas is None:
        c1 = ROOT.TCanvas('c1')
    else:
        c1 = canvas
    
    bg = ROOT.TH2D('bg_%s_%s' % (trigger, scenario),scenario, 1,2.0,10.0, 1,-0.05,0.05)
    bg.SetTitle('A_{LL}(%s) - A_{LL}(no trigger) for #pi^{%s} in %s scenario' % (trigger, charge, scenario))
    bg.SetXTitle('#pi reco p_{T}')
    
    if charge == '+':
        m[trigger][scenario].SetMarkerStyle(20)
    else:
        m[trigger][scenario].SetMarkerStyle(21)        
        
    # plot deviation only
    if trigger != 'notrig':
        for b in range(m[trigger][scenario].GetNbinsX()):
            content = m[trigger][scenario].GetBinContent(b+1) - m['notrig'][scenario].GetBinContent(b+1)
            m[trigger][scenario].SetBinContent(b+1, content)
    else:
        bg.SetTitle('Raw A_{LL} (no trigger) for #pi^{%s} in %s scenario' % (charge, scenario))
        m[trigger][scenario].SetMarkerColor(ROOT.kRed)
        m[trigger][scenario].SetLineColor(ROOT.kRed)
        
    bg.Draw()
    m[trigger][scenario].Draw('p same')
    #raw_input('what do you think?')
    return [f, bg, m[trigger][scenario]]


def asymmetries4(histFile='./combined.plus.hist.root'):
    """calls asymmetries3 for all possible combos of trigger and scenario => save to Postscript"""
    ps = ROOT.TPostScript('blah.ps')
    c = ROOT.TCanvas('c','',100,100,600,800)
    c.Divide(2,3)
    keepMe = []
    for scenario in asymKeys[1:-1]:
        c.Update()
        ps.NewPage()
        c.Clear()
        c.Divide(2,3)
        padCounter = 1
        for trigger in trigKeys:
            pad = c.cd(padCounter)
            keepMe.extend( asymmetries3(histFile, trigger, scenario, pad) )
            padCounter += 1
            print 'generated', trigger, scenario
    
    ps.Close()
            


def validateAsymmetries(histFile='combined.plus.hist.root', subprocess=''):
    '''compare untriggered PYTHIA asymmetries to GRSV predictions'''
    tfile = ROOT.TFile.Open(histFile, 'read')
    miniHist = MiniMcHistos('notrig', subprocess, tfile)
    
    if 'plus' in histFile:
        charge = 'plus'
    elif 'minus' in histFile:
        charge = 'minus'
    else: 
        print 'could not determine charge from filename -- assume plus'
        charge = 'plus'
    
    #import mystyle; mystyle.use(1)
    ROOT.gStyle.SetOptStat(0)
    
    c1 = ROOT.TCanvas('c1')
    
    asymKeys = ['nlo','max','min','zero']
    #if subprocess != '':
    #    asymKeys = [elem + '_' + subprocess for elem in asymKeys]
    
    if charge == 'plus':
        [ miniHist[key].SetMarkerStyle(20) for key in asymKeys ]
        miniHist[ asymKeys[0] ].SetTitle('A_{LL} for #pi^{+} in Pythia and GRSV')
    elif charge == 'minus':
        [ miniHist[key].SetMarkerStyle(21) for key in asymKeys ]        
        miniHist[ asymKeys[0] ].SetTitle('A_{LL} for #pi^{-} in Pythia and GRSV')
    
    [ miniHist[ a ].SetFillStyle( 3004 ) for a in asymKeys ]
    
    miniHist[ asymKeys[0] ].SetLineColor( ROOT.kBlack ) 
    miniHist[ asymKeys[0] ].SetMarkerColor( ROOT.kBlack )   
    miniHist[ asymKeys[0] ].SetFillColor( ROOT.kBlack )   
    miniHist[ asymKeys[0] ].Draw('e4')
    
    miniHist[ asymKeys[0] ].GetYaxis().SetRangeUser(-0.04, 0.08)
    miniHist[ asymKeys[0] ].GetXaxis().SetRangeUser(1.25, 11.0)
    miniHist[ asymKeys[0] ].SetXTitle('p_{T}')
    
    miniHist[ asymKeys[1] ].SetLineColor( ROOT.kRed )
    miniHist[ asymKeys[1] ].SetMarkerColor( ROOT.kRed )
    miniHist[ asymKeys[1] ].SetFillColor( ROOT.kRed )
    miniHist[ asymKeys[1] ].Draw('e4 same')
    
    miniHist[ asymKeys[2] ].SetLineColor( ROOT.kGreen )
    miniHist[ asymKeys[2] ].SetMarkerColor( ROOT.kGreen )
    miniHist[ asymKeys[2] ].SetFillColor( ROOT.kGreen )
    miniHist[ asymKeys[2] ].Draw('e4 same')
    
    miniHist[ asymKeys[3] ].SetLineColor( ROOT.kBlue )
    miniHist[ asymKeys[3] ].SetMarkerColor( ROOT.kBlue )
    miniHist[ asymKeys[3] ].SetFillColor( ROOT.kBlue )
    miniHist[ asymKeys[3] ].Draw('e4 same')
    
    # now draw theory curves
    if subprocess == '':
        import asym, xsec
        #theory = asym.theoryCurves()
        #curves = [  theory.getGraph(charge, 'std'),
        #            theory.getGraph(charge, 'max'),
        #            theory.getGraph(charge, 'zero'),
        #            theory.getGraph(charge, 'min')
        #]
        if charge == 'plus':
            curves = [ 
            asym.theoryCurves( asym.werner_plus_dss_cteqm5_std, xsec.werner_plus_dss_cteqm5_pt ).getGraph(),
            asym.theoryCurves( asym.werner_plus_dss_cteqm5_max, xsec.werner_plus_dss_cteqm5_pt ).getGraph(),
            asym.theoryCurves( asym.werner_plus_dss_cteqm5_zero,xsec.werner_plus_dss_cteqm5_pt ).getGraph(),
            asym.theoryCurves( asym.werner_plus_dss_cteqm5_min, xsec.werner_plus_dss_cteqm5_pt ).getGraph()
            ]
        if charge == 'minus':
            curves = [ 
            asym.theoryCurves( asym.werner_minus_dss_cteqm5_std, xsec.werner_minus_dss_cteqm5_pt ).getGraph(),
            asym.theoryCurves( asym.werner_minus_dss_cteqm5_max, xsec.werner_minus_dss_cteqm5_pt ).getGraph(),
            asym.theoryCurves( asym.werner_minus_dss_cteqm5_zero,xsec.werner_minus_dss_cteqm5_pt ).getGraph(),
            asym.theoryCurves( asym.werner_minus_dss_cteqm5_min, xsec.werner_minus_dss_cteqm5_pt ).getGraph()
            ]
        
        curves[1].SetLineColor(ROOT.kRed)
        curves[2].SetLineColor(ROOT.kBlue)
        curves[3].SetLineColor(ROOT.kGreen)
        [ c.Draw('c same') for c in curves ]
    
    raw_input('what do you think?')
    name = 'raw_asymmetries_' + charge
    if subprocess != '': name = name + '_' + subprocess
    #c1.Print(name + '.gif')


class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg
    


def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        try:
            opts, args = getopt.getopt(argv[1:], 'h', ["help", 'xgrid', 'charge=', 'trig', 'fill=', 'combine'])
        except getopt.error, msg:
            raise Usage(msg)
        
        #global loadLibs 
        #loadLibs = localLibs
        global site
        
        charge = 0
        
        # option processing
        for option, value in opts:
            #if option == '--xgrid':             
            #    loadLibs = xgridLibs
            #    site = 'Xgrid'
            if option == '--charge':
                charge = int(value)
            if option in ("-h", "--help"):      raise Usage(help_message)
            if option == '--trig':              makeTriggerNtuple2(args)
            if option == '--fill':              
                #loadLibs()
                #fill2(value,None,True,os.environ['FILELIST'])
                fill3(value)
            if option == '--combine':
                #loadLibs()
                ROOT.gSystem.Load('StJetMaker')
                ROOT.gSystem.Load('StMiniMcEvent')
                combine(args[0],args[1:],True)
            
    
    except Usage, err:
        print >> sys.stderr, sys.argv[0].split("/")[-1] + ": " + str(err.msg)
        print >> sys.stderr, "\t for help use --help"
        return 2


if __name__ == "__main__":
    #test()
    sys.exit(main())
