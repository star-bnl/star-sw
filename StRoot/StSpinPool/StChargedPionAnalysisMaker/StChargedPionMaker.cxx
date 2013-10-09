// $Id: StChargedPionMaker.cxx,v 1.23 2013/10/09 14:43:39 stevens4 Exp $

#include "StChargedPionMaker.h"

//ROOT headers
#include "TFile.h"
#include "TTree.h"
#include "TClonesArray.h"
#include "TChain.h"

//StarClassLibrary
#include "SystemOfUnits.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

//logger
#include "StMessMgr.h"

//my headers
#include "StChargedPionEvent.h"
#include "StChargedPionVertex.h"
#include "StChargedPionTrack.h"
#include "StChargedPionJet.h"
#include "StChargedPionJetParticle.h"
#include "StChargedPionMcEvent.h"
#include "StChargedPionHelpers.h"
#include "StChargedPionTypes.h"

//StDetectorDbMaker
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"

//StJetMaker
#include "StSpinPool/StJetSkimEvent/StJetSkimEvent.h"
#include "StSpinPool/StJets/StJets.h"
#include "StSpinPool/StJets/StJet.h"
#include "StJetMaker/StJetMaker.h"

//StSpinDbMaker
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"

//StEmcTriggerMaker
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"

//StTriggerUtilities
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

//StMiniMcEvent
#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StTinyMcTrack.h"
#include "StMiniMcEvent/StMiniMcPair.h"

#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

//StMcEvent
#include "StMcEvent/StMcEvent.hh"

ClassImp(StChargedPionMaker)

StChargedPionMaker::StChargedPionMaker(const char *name, const char *outputfile) : 
    StMaker(name) 
{
    LOG_INFO << "calling constructor" << endm;
    
    mFile = new TFile(outputfile,"RECREATE");

    mBadTracks = new TH1D("badTracks","tracks failing quality cuts",4,0.5,4.5);

    time_t rawtime;
    time(&rawtime);
    char title[200];
    sprintf(title, "created %s", ctime(&rawtime));
    mTree = new TTree("tree",title);
    
    bool isMonteCarlo = GetMakerInheritsFrom("StEmcSimulatorMaker") ? true:false;
    LOG_INFO << "Are we running on Monte Carlo? " << isMonteCarlo << endm;
    
    if(isMonteCarlo) {
        mEvent = new StChargedPionMcEvent();
        mTree->Branch("event", "StChargedPionMcEvent", &mEvent);
    }
    else {
        mEvent = new StChargedPionEvent();
        mTree->Branch("event", "StChargedPionEvent", &mEvent);
    }
    
    Long64_t autosave = 1000000000; //1GB
    mTree->SetAutoSave(autosave);
    mTree->SetMaxTreeSize(autosave);
    
    mMuDstMk      = NULL;
    mSpDbMk       = NULL;
    mEmcTrgMk     = NULL;
    mJetMk        = NULL;
    mTrgSimuMk   = NULL;
    
    mJetFile = NULL;
    mJetTree = NULL;
    mJets = new StJets();
    mPyJets = new StJets();
    
    mMiniMcFile = NULL;
    mMiniMcTree = NULL;
    mMiniMcEvent = new StMiniMcEvent();
    
    mTriggers.clear();
    
    LOG_INFO << "finished constructor" << endm; 
}

StChargedPionMaker::~StChargedPionMaker() {
    LOG_DEBUG << "calling destructor" << endm;
    
    if(mEvent) delete mEvent;
    
    if(mJetFile) mJetFile->Close();
    delete mJets;
    delete mPyJets;
    
    if(mMiniMcFile) mMiniMcFile->Close();
    delete mMiniMcEvent;
    
    LOG_DEBUG << "finished destructor" << endm;
}

void StChargedPionMaker::Clear(const char*) {
    StChargedPionEvent   *data = dynamic_cast<StChargedPionEvent*>(mEvent);
    StChargedPionMcEvent *simu = dynamic_cast<StChargedPionMcEvent*>(mEvent);
    if(data) data->Clear();
    if(simu) simu->Clear();
    StMaker::Clear();
}

Int_t StChargedPionMaker::Init() {
    mMuDstMk    = dynamic_cast<StMuDstMaker*>(
                        GetMakerInheritsFrom("StMuDstMaker"));
    mSpDbMk     = dynamic_cast<StSpinDbMaker*>(
                        GetMakerInheritsFrom("StSpinDbMaker"));
    mEmcTrgMk   = dynamic_cast<StEmcTriggerMaker*>(
                        GetMakerInheritsFrom("StEmcTriggerMaker")); 
    mJetMk      = dynamic_cast<StJetMaker*>(
                        GetMakerInheritsFrom("StJetMaker"));
    mTrgSimuMk  = dynamic_cast<StTriggerSimuMaker*>(
                        GetMakerInheritsFrom("StTriggerSimuMaker"));
    
    // do the setup for mPyJets here, since we only run on one file at a time
    if( dynamic_cast<StChargedPionMcEvent*>(mEvent) ) {
        if(mJetMk) {
	    mPyJets = mJetMk->getStJets("PythiaConeJets");
            LOG_INFO << "loaded mPyJets at " << mPyJets << endm;
        }
    }
    
    this->Clear();
    
    LOG_INFO << "init OK" << endm;
    return StMaker::Init();
}

Int_t StChargedPionMaker::InitRun(int runnumber) {
    if(mJetMk) {
        LOG_INFO << "found StJetMaker in the chain" << endm;
        if(StJets* stjets = mJetMk->getStJets("ConeJets")) {
	    mJets = stjets;
            LOG_INFO << "found Jets in Run 5 branch " << mJets << endm;
        }
        else if(StJets* stjets = mJetMk->getStJets("ConeJets12")) {
	    mJets = stjets;
            LOG_INFO << "found Jets in Run 6 branch " << mJets << endm;
        }
        else {
	    mJets = mJetMk->getStJets("ConeJets12_0.7");
            LOG_INFO << "found ConeJets12_0.7 branch " << mJets << endm;
        }
    }
    else {
        LOG_INFO << "trying to get the jets off disk" << endm;
        std::ostringstream os;
        if(runnumber < 7000000) {
            os << "/star/institutions/mit/common/run5/jets/jets_" << runnumber << ".tree.root";
        }
        else {
            // best confirm this one with Murad first
            os << "/star/institutions/mit/common/run6/jets/jets_" << runnumber << ".tree.root";
        }
        
        if(mJetFile) mJetFile->Close();
        mJetTree = NULL;
        mJetFile = TFile::Open(os.str().c_str());
        if(mJetFile) mJetTree = (TTree*) mJetFile->Get("jet");
        if(mJetTree) {
            if(runnumber < 7000000) {
                mJetTree->SetBranchAddress("ConeJets", &mJets);            
            }
            else {
                mJetTree->SetBranchAddress("ConeJets12", &mJets);            
            }
            mJetTree->BuildIndex("mRunId","mEventId");
        }
    }
    
    return StMaker::InitRun(runnumber);
}

Int_t StChargedPionMaker::Make()
{
    StChargedPionEvent   *data = dynamic_cast<StChargedPionEvent*>(mEvent);
    StChargedPionMcEvent *simu = dynamic_cast<StChargedPionMcEvent*>(mEvent);
    
    //have we changed files?
    TString inputFile(mMuDstMk->chain()->GetFile()->GetName());
    if(mCurrentFile != inputFile){
        mCurrentFile = inputFile;
        const char *baseName = strrchr(mCurrentFile.Data(), '/');
        mEvent->setMuDstName( baseName );
        
        if(simu) {
            TString minimcFile = inputFile.ReplaceAll("MuDst","minimc");
            mMiniMcFile = TFile::Open(minimcFile);
            if(mMiniMcFile) {
                LOG_INFO << "opened minimc file at " << minimcFile << endm;
                mMiniMcTree = dynamic_cast<TTree*> (mMiniMcFile->Get("StMiniMcTree"));
                mMiniMcTree->BuildIndex("mEventId");
                mMiniMcTree->SetBranchAddress("StMiniMcEvent", &mMiniMcEvent);
            }
            else {
                LOG_WARN << "problem opening minimc at " << minimcFile << endm;
            }
        }
    }
    
    if(data) {
        StChargedPionHelpers::translateMuDst(data);
        
        // triggers, prescales
        mTriggers.clear();
        map<int,float> m = StDetectorDbTriggerID::instance()->getTotalPrescales();
        for (map<int,float>::iterator it=m.begin(); it!=m.end(); ++it) {
            int trigId = it->first;
            data->setPrescale(trigId, it->second);
                        
            if( StMuDst::event()->triggerIdCollection().nominal().isTrigger(trigId) ) {
                data->addTrigger(trigId);
            }
            
            mTriggers.push_back(trigId);
        }
        
        makeTriggerSimu(data);
        
        //spin DB
        int bx48 =  StMuDst::event()->l0Trigger().bunchCrossingId();
        data->setSpinBit( mSpDbMk->spin4usingBX48(bx48) );
        data->setPolValid( mSpDbMk->isValid() );
        data->setPolLong( mSpDbMk->isPolDirLong() );
        data->setPolTrans( mSpDbMk->isPolDirTrans() );
        data->setBxingMasked( mSpDbMk->isMaskedUsingBX48(bx48) );
        data->setBxingOffset( mSpDbMk->offsetBX48minusBX7(bx48, data->bx7()) );
    }
    
    if(simu) {
        StChargedPionHelpers::translateMuDst(simu);
        
        makeTriggerSimu(simu);
        
        StMcEvent *mcEvent = static_cast<StMcEvent*>( GetDataSet("StMcEvent") );
        simu->setProcessId( mcEvent->subProcessId() );
        
        TDataSet *Event = GetDataSet("geant");
        TDataSetIter geantDstI(Event);

        St_g2t_pythia* PyPtr = (St_g2t_pythia *) geantDstI("g2t_pythia");
        g2t_pythia_st *g2t_pythia = PyPtr->GetTable();
        simu->setHardP( g2t_pythia->hard_p );
        simu->setX1( g2t_pythia->bjor_1 );
        
        St_particle *particleTabPtr = (St_particle *) geantDstI("particle");
        particle_st* particleTable = particleTabPtr->GetTable();
        
        simu->isr1().SetXYZT( 
            particleTable[2].phep[0],
            particleTable[2].phep[1],
            particleTable[2].phep[2],
            particleTable[2].phep[3]
            );
        
        simu->isr2().SetXYZT(
            particleTable[3].phep[0],
            particleTable[3].phep[1],
            particleTable[3].phep[2],
            particleTable[3].phep[3]
            );
        
        simu->parton1().SetXYZT(
            particleTable[4].phep[0],
            particleTable[4].phep[1],
            particleTable[4].phep[2],
            particleTable[4].phep[3]
            );
        
        simu->parton2().SetXYZT(
            particleTable[5].phep[0],
            particleTable[5].phep[1],
            particleTable[5].phep[2],
            particleTable[5].phep[3]
            );

        simu->parton3().SetXYZT(
            particleTable[6].phep[0],
            particleTable[6].phep[1],
            particleTable[6].phep[2],
            particleTable[6].phep[3]
            );

        simu->parton4().SetXYZT(
            particleTable[7].phep[0],
            particleTable[7].phep[1],
            particleTable[7].phep[2],
            particleTable[7].phep[3]
            );

        simu->setFlavor(1, particleTable[4].idhep);
        simu->setFlavor(2, particleTable[5].idhep);
        simu->setFlavor(3, particleTable[6].idhep);
        simu->setFlavor(4, particleTable[7].idhep);
        
        // save all final-state particles with E > 2.0 GeV
        for(int i = 8; i < particleTabPtr->GetNRows(); ++i)
        {
            if(particleTable[i].isthep == 1 && particleTable[i].phep[3] > 2.0)
            {
                StChargedPionPythiaRow r;
                r.id = particleTable[i].idhep;
                r.vec = StChargedPionLorentzVector(particleTable[i].phep[0],
                                                   particleTable[i].phep[1],
                                                   particleTable[i].phep[2],
                                                   particleTable[i].phep[3]);
                simu->pythiaRecord().push_back(r);
            }
        }

        int bytesRead = mMiniMcTree->GetEntryWithIndex(simu->eventId());
        if(bytesRead > 0) {
            StChargedPionHelpers::translateMinimc(mMiniMcEvent, simu);
        }

        // still need to set the the pythia jets
        if(mJetMk) {
            StChargedPionHelpers::translateJets(mPyJets, simu);
        }
        else if(mJetTree) {
            int ok = mJetTree->GetEntryWithIndex(simu->runId(), simu->eventId());
            if(ok > 0) StChargedPionHelpers::translateJets(mJets, simu);
        }
    }

    //and the jets
    if(mJetMk) {
        StChargedPionHelpers::translateJets(mJets, mEvent);
    }
    else if(mJetTree) {
        int ok = mJetTree->GetEntryWithIndex(mEvent->runId(), mEvent->eventId());
        if(ok > 0) StChargedPionHelpers::translateJets(mJets, mEvent);
    }
    
    mTree->Fill();
    
    return StMaker::Make();
}

Int_t StChargedPionMaker::Finish() {
    mFile->cd();
    mTree->Write();
    mBadTracks->Write();
    mFile->Close();
    LOG_INFO << "finished OK"<<endm;
    return StMaker::Finish();
}

void StChargedPionMaker::makeTriggerSimu(StChargedPionBaseEv *ev) {
    if(mTrgSimuMk) {
        for (unsigned i=0; i<mTriggers.size(); ++i) {
            int trigId = mTriggers[i];
            
            if( mTrgSimuMk->isTrigger(trigId) ) {
                ev->addSimuTrigger(trigId);
            }
            
            const StTriggerSimuResult result = mTrgSimuMk->detailedResult(trigId);
            for(unsigned i=0; i<result.highTowerIds().size(); i++) {
                int tid = result.highTowerIds().at(i);
                ev->addHighTower(tid, result.highTowerAdc(tid));
            }
            for(unsigned i=0; i<result.triggerPatchIds().size(); i++) {
                int tid = result.triggerPatchIds().at(i);
                ev->addTriggerPatch(tid, result.triggerPatchAdc(tid));
            }
            for(unsigned i=0; i<result.jetPatchIds().size(); i++) {
                int tid = result.jetPatchIds().at(i);
                ev->addJetPatch(tid, result.jetPatchAdc(tid));
            }
            ev->setL2Result(result.l2Result(kJet), true);
        }
    }
    else if(mEmcTrgMk) {
        //minbias simu trigger
        int Npmt=StMuDst::event()->bbcTriggerDetector().numberOfPMTs();
        bool eastBBC(false), westBBC(false);
        for (int pmt=0;pmt<Npmt;pmt++){
            if(StMuDst::event()->bbcTriggerDetector().adc(pmt) > 5) {
                if(pmt<16) eastBBC = true;
                if(pmt>23 && pmt<40) westBBC = true;
            }
        }
        if(eastBBC && westBBC) {
            ev->addSimuTrigger(96011);  
            ev->addSimuTrigger(117011);
        }
        
        for (unsigned i=0; i<mTriggers.size(); ++i) {
            int trigId = mTriggers[i];
            
            if ( mEmcTrgMk->isTrigger(trigId) ) {
                ev->addSimuTrigger(trigId);
            }
            
            map<int,int> m( mEmcTrgMk->barrelTowersAboveThreshold(trigId) );
            for(map<int,int>::const_iterator iter=m.begin(); iter!=m.end(); iter++) {
                ev->addHighTower(iter->first, iter->second);
            }
            
            m = mEmcTrgMk->barrelTriggerPatchesAboveThreshold(trigId);
            for(map<int,int>::const_iterator iter=m.begin(); iter!=m.end(); iter++) {
                ev->addTriggerPatch(iter->first, iter->second);
            }
            
            m = mEmcTrgMk->barrelJetPatchesAboveThreshold(trigId);
            for(map<int,int>::const_iterator iter=m.begin(); iter!=m.end(); iter++) {
                ev->addJetPatch(iter->first, iter->second);
            }
        }
    }
}

/*****************************************************************************
 * $Log: StChargedPionMaker.cxx,v $
 * Revision 1.23  2013/10/09 14:43:39  stevens4
 * Add const to char* in 2 lines to compile on 5.34.09 and SL6.4 on rplay18
 *
 * Revision 1.22  2010/04/25 16:07:57  pibero
 * Modified StChargedPionMaker.cxx to use the safer StJetMaker::getStJets()
 * instead of the outdated and no longer available StJetMaker::getJets().
 *
 * Revision 1.21  2010/04/25 15:18:49  pibero
 * Temporary fix to keep AutoBuild happy. Permanent solution to be posted soon.
 *
 * Revision 1.20  2009/06/22 14:44:50  kocolosk
 * support for new ConeJets12_0.7 branch name
 *
 * Revision 1.19  2009/04/02 18:25:42  kocolosk
 * fixed paths to jet codes
 *
 * Revision 1.18  2008/12/29 15:58:30  kocolosk
 * removed commented code and added Id and Log as needed
 *
 * Revision 1.17  2008/08/25 20:55:35  kocolosk
 * get correct ConeJets/ConeJets12 branch w/o using runnumber
 *
 * Revision 1.16  2008/07/17 17:06:30  kocolosk
 * big-bang integration StChargedPionMcEvent framework
 *
 *****************************************************************************/
