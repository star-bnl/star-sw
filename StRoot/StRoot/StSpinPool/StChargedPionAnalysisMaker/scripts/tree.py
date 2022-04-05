import os
from datetime import datetime
import ROOT

skimDir  = '/Volumes/scratch/common/run6/jetSkim'
trackDir = '/Volumes/scratch/common/run6/chargedPions'
jetDir   = '/Volumes/scratch/common/run6/jets'

def fromJetSkim(runnumber):
    """
    save an StChargedPionEvent tree in $PWD, optionally including tracks & jets
    """
    skimFile = ROOT.TFile('%s/jetSkim_%d.tree.root' % (skimDir, runnumber))
    
    trackFile = ROOT.TFile('%s/chargedPions_%d.tree.root' % (trackDir,runnumber))
    try:
        trackFile.chargedPionTree.BuildIndex('run','event')
    except AttributeError:
        print 'problem with charged pion tree in', runnumber
        return
    
    jetFile = ROOT.TFile('%s/jets_%d.tree.root' % (jetDir, runnumber))
    try:
        jetFile.jet.SetBranchStatus('ConeJets5*', 0)
        jetFile.jet.SetBranchStatus('ConeJetsEMC*', 0)
        jetFile.jet.BuildIndex('mRunId','mEventId')
    except AttributeError:
        print 'problem with jet tree in', runnumber
        return
    jetFile.jet.GetEntry(0)
    try:
        ConeJets = jetFile.jet.ConeJets
    except AttributeError:
        ConeJets = jetFile.jet.ConeJets12
        
    print '%d : skim=%d track=%d jet=%d' % (runnumber, \
        skimFile.jetSkimTree.GetEntries(), \
        trackFile.chargedPionTree.GetEntries(), jetFile.jet.GetEntries())
    
    outFile = ROOT.TFile('chargedPions_%d.tree.root' % (runnumber,), 'recreate')
    outTree = ROOT.TTree('tree', 'created %s' % (str(datetime.now()),))
    outTree.SetAutoSave(100000000)
    ev = ROOT.StChargedPionEvent()
    outTree.Branch('event','StChargedPionEvent',ev)
     
    for entry in skimFile.jetSkimTree:
        # event info
        sk = skimFile.jetSkimTree.skimEventBranch
        ROOT.StChargedPionMaker.translateEvent(sk, ev)
        
        # charged pion info
        trackFile.chargedPionTree.GetEntryWithIndex(sk.runId(), sk.eventId())
        try:
            assert(trackFile.chargedPionTree.event == ev.eventId())
        except AssertionError:
            print runnumber, 'is missing some data from', sk.mudstFileName()
            return
        
        for track in trackFile.chargedPionTree.primaries:
           ev.addTrack(track)
            
        # jet info using maker.translateJets
        jetFile.jet.GetEntryWithIndex(sk.runId(), sk.eventId())
        assert(ConeJets.eventId() == ev.eventId())
        ROOT.StChargedPionMaker.translateJets(ConeJets, ev)
        
        outTree.Fill()
        ev.Clear()
    
    outFile.cd()
    outTree.Write()
    outFile.Close()


def insertTracks():
    """clones this tree into a new file in $PWD and replaces tracks"""
    pass


def insertJets():
    """clones this tree into a new file in $PWD and replaces jets"""
    pass


def measureReadSpeed():
    import time
    chain = ROOT.TChain('tree')
    
    ROOT.StChargedPionJetParticle.Class().IgnoreTObjectStreamer()
    chain.Add('~/work/charged-pion-event-2/chargedPions_61*')
    
    chain.SetBranchStatus('mJets*',0)
    #chain.SetBranchStatus('mJets.mParticles*',0)
    
    #elistFile = ROOT.TFile('~/work/charged-pion-event/eventLists.root')    
    elistFile = ROOT.TFile('~/work/charged-pion-event-2/eventLists.root')
    elist = elistFile.Get('all_cuts')
    
    counter = 0
    beginTime = time.clock()
    for i in range(elist.GetN()):
        if chain.GetEntry(elist.GetEntry(i)) == 0: break
        #for entry in chain:
        ev = chain.event
        if counter % 1000 == 1: print ev.runId(), ev.eventId()
        counter += 1
        mb = ev.isTrigger(96011) and ev.isSimuTrigger(96011)
        nVerts = ev.nVertices()
        for jet in ev.jets():
            pt = jet.Pt()
            for particle in jet.particles():
                det = particle.detectorId()
        for track in ev.tracks():
            eta = track.Eta()
    endTime = time.clock()
    print 'Processed', counter, ' events in ', endTime-beginTime, ' CPU seconds'
        
    
    print 'Goodbye'


def integratedLuminosity(tree, minbiasId=96011):
    """
    returns recorded integrated luminosity seen by minbias trigger in nb^-1
    """
    bbcXsec = 26.1 * 1e6 ## in nanobarns
    
    tree.SetBranchStatus('*', 0)
    tree.SetBranchStatus('mTriggerBits', 1)
    
    tree.SetBranchStatus('mTriggerPrescales', 1)
    tree.GetEntry(0)
    prescale = tree.event.prescale(minbiasId)
    tree.SetBranchStatus('mTriggerPrescales', 0)
    
    mbEventCounter = 0
    for entry in tree:
        if entry.event.isTrigger(minbiasId): mbEventCounter += 1
    return (mbEventCounter * prescale) / bbcXsec


def makeEventLists(fname):
    #ROOT.StChargedPionJetParticle.Class().IgnoreTObjectStreamer()
    #f = ROOT.TFile(fname, 'update')
    #ch = f.tree
    ch = ROOT.TChain('tree')
    ch.Add('~/data/run5/tree/chargedPions_*.tree.root')
    
    elists = []
    
    outFile = ROOT.TFile('eventLists.root','recreate')
    
    print 'generating has_trigger list'
    trigIds = [96011,96201,96211,96221,96233]
    trigString = ''
    for trig in trigIds[:-1]:
        trigString += 'event.isTrigger(%d) || ' % (trig,)
    trigString += 'event.isTrigger(%d)' % (trigIds[-1])
    ch.Draw('>>has_trigger',trigString)
    elists.append( ROOT.gDirectory.Get('has_trigger') )
    
    print 'generating has_vertex list'
    ch.Draw('>>has_vertex','mVertices.mRanking>0')
    elists.append( ROOT.gDirectory.Get('has_vertex') )
    
    print 'generating bbc_789 list'
    #ch.Draw('>>bbc_789','mBbcTimeBin%32==0 && mBbcTimeBin/32>6 && \
    #   mBbcTimeBin/32<9')
    ## dropping the discrete timebin cut -- APK 2008-01-09
    ## also fix a BUG!!! 9 vs. 10
    ch.Draw('>>bbc_789','mBbcTimeBin%32==0 && mBbcTimeBin/32>6 && \
        mBbcTimeBin/32<10')
    elists.append( ROOT.gDirectory.Get('bbc_789') )
    
    print 'generating spinDbOk list'
    ch.Draw('>>spinDbOk','event.isSpinValid()')
    elists.append( ROOT.gDirectory.Get('spinDbOk') )
    
    print 'generating has_track list'
    ch.Draw('>>has_track','mTracks.fE>0')
    elists.append( ROOT.gDirectory.Get('has_track') )
    
    print 'generating has_good_pion list'
    ch.Draw('>>has_good_pion','abs(mTracks.eta())<1.0 && \
        mTracks.globalDca().mag()<1.0 && mTracks.nHitsFit()>25 && \
        mTracks.nSigmaPion()>-1 && mTracks.nSigmaPion()<2')
    elists.append( ROOT.gDirectory.Get('has_good_pion') )
    
    print 'now get intersection'
    all_cuts = ROOT.TEventList('all_cuts','intersection of above lists')
    all_cuts.Add( elists[0] )
    print elists
    for e in elists: 
        print e.GetName(), e.GetN()
        all_cuts.Intersect(e)
    print all_cuts.GetName(), all_cuts.GetN()
    
    outFile.cd()
    [e.Write() for e in elists]
    all_cuts.Write()
    outFile.Close()


if __name__ == '__main__':
    print 'nothing defined in main function at the moment'