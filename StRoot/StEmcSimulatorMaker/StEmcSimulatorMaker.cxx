// $Id: StEmcSimulatorMaker.cxx,v 1.46 2007/09/12 02:55:15 kocolosk Exp $

#include "StEmcSimulatorMaker.h"

#include <assert.h>
#include <set>

#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StMcEvent/StMcEmcModuleHitCollection.hh"
#include "StMcEvent/StMcEmcHitCollection.hh"
#include "StMcEvent/StMcEvent.hh"

#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEvent.h"

#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StEmcSimpleSimulator.h"
#include "StEmcPmtSimulator.h"

ClassImp(StEmcSimulatorMaker)

StEmcSimulatorMaker::StEmcSimulatorMaker(const char *name):StMaker(name) {
    // checking if this is embedding mode by looking for StEmcRawMaker in this chain
    mEmbeddingMode = GetMaker("emcEmbed") ? true:false;
    LOG_INFO <<"StEmcSimulatorMaker EMBEDDING mode = "<< (Int_t)mEmbeddingMode <<endm;
    
    // initialize control table
    for(int i=0; i<MAXDETBARREL; i++) { 
        mMakeFullDetector[i]    = (mEmbeddingMode) ? false:true;
        mCheckStatus[i]         = true;
        mDoZeroSuppression[i]   = (mEmbeddingMode) ? false:true;
        mPedestalCut[i]         = 1.5;
        mCalibOffset[i]         = 0.0;
        mCalibSpread[i]         = 0.0;
    }
    
    mMcEvent = NULL;
    mEmcCollection = NULL;
    
    for(int i=0; i<MAXDETBARREL; i++) {
        mEmcMcHits[i] = NULL;
        mGeom[i] = StEmcGeom::instance(i+1);
        if(!mGeom[i]) {
            LOG_FATAL << "Geometry for detector "<<i+1<<" undefined" << endm;
            assert(0);
        }
    }
    
    mTables = new StBemcTables(kTRUE, kTRUE);
    
    // simulators are instantiated in Init, but we set some params here in case the user wants to change them
    mSimulatorMode[BTOW-1]  = StEmcVirtualSimulator::kPrimarySecondaryFullMode;
    mSimulatorMode[BPRS-1]  = StEmcVirtualSimulator::kPrimarySecondaryFullMode;
    mSimulatorMode[BSMDE-1] = StEmcVirtualSimulator::kSimpleMode;
    mSimulatorMode[BSMDP-1] = StEmcVirtualSimulator::kSimpleMode;    
}

StEmcSimulatorMaker::~StEmcSimulatorMaker() {
    delete mSimulator[BTOW-1];
    delete mSimulator[BPRS-1];
    delete mSimulator[BSMDE-1];
    delete mSimulator[BSMDP-1];
    
    delete mTables;
}

Int_t StEmcSimulatorMaker::Init() {
    mSimulator[BTOW-1]  = new StEmcPmtSimulator(kBarrelEmcTowerId, mSimulatorMode[BTOW-1]);
    mSimulator[BPRS-1]  = new StEmcPmtSimulator(kBarrelEmcPreShowerId, mSimulatorMode[BPRS-1]);
    mSimulator[BSMDE-1] = new StEmcSimpleSimulator(kBarrelSmdEtaStripId, mSimulatorMode[BSMDE-1]);
    mSimulator[BSMDP-1] = new StEmcSimpleSimulator(kBarrelSmdPhiStripId, mSimulatorMode[BSMDP-1]);
    
    for(int det=BTOW; det<=BSMDP; det++) {
        mSimulator[det-1]->setEmbeddingMode(mEmbeddingMode);
        mSimulator[det-1]->setTables(mTables);
        mSimulator[det-1]->setCalibScale(1.0 + mCalibOffset[det-1]);
        mSimulator[det-1]->setCalibSpread(mCalibSpread[det-1]);
    }
    
    return kStOk;
}

void StEmcSimulatorMaker::Clear(const char*) {
    mMcEvent = NULL;
}

Int_t StEmcSimulatorMaker::Make() {
    mMcEvent = dynamic_cast<StMcEvent*>(GetDataSet("StMcEvent"));
    if(mMcEvent) {
        mEmcMcHits[0] = mMcEvent->bemcHitCollection();
        mEmcMcHits[1] = mMcEvent->bprsHitCollection();
        mEmcMcHits[2] = mMcEvent->bsmdeHitCollection();
        mEmcMcHits[3] = mMcEvent->bsmdpHitCollection();
    }
    else {
        LOG_ERROR << "couldn't find StMcEvent for this event" << endm;
        return kStErr;
    }
    
    //simulate pedestals where no hit was found if makeFullDetector is specified
    int maxChannels[4] = {4800, 4800, 18000, 18000};
    std::vector<int> hasHit;
    std::vector<int>::const_iterator iter;
    
    int softId, module, eta, sub;
    for(int det=BTOW; det<=BSMDP; det++) {
        if(mMakeFullDetector[det-1] == 0) continue;
        
        //identify the channels that already have real hits        
        for(unsigned int mod=1; mod<=mEmcMcHits[det-1]->numberOfModules(); mod++) {
            const StMcEmcModuleHitCollection* module = mEmcMcHits[det-1]->module(mod);
            const vector<StMcCalorimeterHit*> hits   = module->hits();
            for(unsigned long i=0; i<module->numberOfHits(); i++) {
                mGeom[det-1]->getId(hits[i]->module(), hits[i]->eta(), hits[i]->sub(), softId);
                hasHit.push_back(softId);
            }
        }
        
        //sort the vector for constant-time access while looping through all channels
        std::sort(hasHit.begin(), hasHit.end());
        iter = hasHit.begin();
        int nextHitId = 0;
        if(hasHit.size()) nextHitId = *iter;
        
        StMcCalorimeterHit *mcHit = new StMcCalorimeterHit();
        
        //add hits with 0 energy to channels that don't have real hits
        for(int softId=1; softId<=maxChannels[det-1]; softId++) {
            if( softId != nextHitId ) {
                mGeom[det-1]->getBin(softId,module,eta,sub);
                mcHit->setModule(module);
                mcHit->setEta(eta);
                mcHit->setSub(sub);
                mcHit->setdE(0.0);
                mcHit->setParentTrack(NULL);
                
                StMcEmcHitCollection::EAddHit retCode = mEmcMcHits[det-1]->addHit(mcHit);
                if(retCode == StMcEmcHitCollection::kNew) {
                    mcHit = new StMcCalorimeterHit(); // StMcEvent owns the old hit now
                }
                else {
                    LOG_WARN << "Tried to simulate ped noise for det=" << det << " softId=" << softId 
                        << " but retCode != kNew.  Instead retCode=" << retCode << endm;
                }
            }
            else {
                iter++;
                if(iter != hasHit.end()) nextHitId = *iter;
            }
        }
        
        delete mcHit;
        hasHit.clear();
        
        //lots of LOG_DEBUG statements
        mEmcMcHits[det-1]->print();
    }
    
    //now convert the energy depositions to ADC values
    makeRawHits();

    return StMaker::Make();
}

// translate each StMcCalorimeterHit into an StEmcRawHit and save in StEmcCollection
void StEmcSimulatorMaker::makeRawHits() {
    mTables->loadTables(this);
    
    // get a valid StEmcCollection to store the hits
    if(mEmbeddingMode) {
        mEmcCollection = new StEmcCollection();
    }
    else {
        StEvent* event = (StEvent*)GetInputDS("StEvent");
        if(event == NULL) {
            event = new StEvent();
            AddData(event);
        }
        mEmcCollection = event->emcCollection();
        if(mEmcCollection == NULL) {
            mEmcCollection = new StEmcCollection();
            event->setEmcCollection(mEmcCollection);
        }
    }
    
    for(int det=BTOW; det<=BSMDP; det++) {
        StDetectorId detectorId;
        switch(det) {
            case BTOW:  detectorId = kBarrelEmcTowerId; break;
            case BPRS:  detectorId = kBarrelEmcPreShowerId; break;
            case BSMDE: detectorId = kBarrelSmdEtaStripId; break;
            case BSMDP: detectorId = kBarrelSmdPhiStripId; break;
        }
        StEmcDetector *detector = new StEmcDetector(detectorId, 120); // should this be a magic number?
        mEmcCollection->setDetector(detector);
        
        for(unsigned int mod=1; mod<=mEmcMcHits[det-1]->numberOfModules(); mod++) {
            const StMcEmcModuleHitCollection* module = mEmcMcHits[det-1]->module(mod);
            const vector<StMcCalorimeterHit*> hits   = module->hits();
            for(unsigned long i=0; i<module->numberOfHits(); i++) {
                int softId; mGeom[det-1]->getId(hits[i]->module(), hits[i]->eta(), hits[i]->sub(), softId);
                
                LOG_DEBUG << "-----------------------------------------------------------------------------------------------" << endm;
                
                // check hit status
                if(mCheckStatus[det-1]) { 
                    if(mTables->status(det, softId) != 1) { 
                        LOG_DEBUG << Form("det=%2d  softId=%5d -- removing hit b/c DB status!=1", detectorId, softId) << endm;
                        continue;
                    }
                }
                
                StEmcRawHit *rawHit = mSimulator[det-1]->makeRawHit(hits[i]);
                rawHit->setCalibrationType(0);
                
                // do zero suppression
                if(mDoZeroSuppression[det-1]) { 
                    float pedMean = mTables->pedestal(det, softId);
                    float pedRMS  = mTables->pedestalRMS(det, softId);
                    if((rawHit->adc() - pedMean) < (mPedestalCut[det-1] * pedRMS)) {
                        LOG_DEBUG << "removing hit b/c it failed pedestal cut" << endm;
                        delete rawHit;
                        continue;
                    }
                }
                
                detector->addHit(rawHit);
            } // loop over hits
        } // loop over modules
    } // loop over detectors
}

/*****************************************************************************
 *  $Log: StEmcSimulatorMaker.cxx,v $
 *  Revision 1.46  2007/09/12 02:55:15  kocolosk
 *  don't do zero suppression on embedding hits (they have no pedestal)
 *
 *  Revision 1.45  2007/09/11 21:49:14  kocolosk
 *  complete overhaul of the BEMC simulator
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2486.html
 *
 *  Revision 1.44  2007/08/06 22:55:56  kocolosk
 *  fixed a logic error in logging that was causing segfaults (RT #1012)
 * 
 *  Revision 1.43  2007/07/13 13:44:20  fisyak
 *  Delete mEmcCollection if set mEmbed
 * 
 *  Revision 1.42  2007/04/05 19:04:33  kocolosk
 *  fix AutoBuild warning
 * 
 *  Revision 1.41  2007/03/22 22:48:28  perev
 *  Small old bug fix, thanx to Oleksandr
 * 
 *  Revision 1.40  2007/03/22 21:51:36  perev
 *  Leak of StMcCalorimeterHit fix
 * 
 *  Revision 1.39  2007/01/23 20:38:59  kocolosk
 *  logger update
 * 
 *  Revision 1.38  2007/01/23 20:36:25  kocolosk
 *  oops ... keyDb should have been keyDB in rev.1.37
 * 
 *  Revision 1.37  2007/01/23 20:14:21  kocolosk
 *  added code in Init() toautomatically set embedding mode controlTable flags if StEmcADCtoEMaker and/or StEmcMixerMaker.  Users do not need to do this in their own macros any more.
 * 
 *  Revision 1.36  2007/01/23 19:44:24  kocolosk
 *  few additional logger fixes
 * 
 *  Revision 1.35  2007/01/22 19:13:40  kocolosk
 *  use STAR logger for all output
 * 
 *  Revision 1.34  2006/09/20 13:44:25  kocolosk
 *  fix autobuild warnings
 * 
 *  Revision 1.33  2006/02/16 16:11:41  suaide
 *  small modification in the way the calibration spread/offset is created
 * 
 *  Revision 1.32  2006/01/24 16:31:47  suaide
 *  disabled printout
 * 
 *  Revision 1.31  2005/05/13 15:49:36  suaide
 *  set correct StEmcRawHit::calibrationType() for simulated hits
 * 
 *  Revision 1.30  2005/03/21 21:36:39  suaide
 *  fixed problem with chain
 * 
 *  Revision 1.29  2005/01/07 11:31:20  suaide
 *  small bug fixed
 * 
 *  Revision 1.28  2004/08/09 19:43:28  suaide
 *  moved global variables to private members and
 *  made small modifications to run in embedding mode
 * 
 *  Revision 1.27  2004/08/06 13:24:48  suaide
 *  New features added and fixed some bugs in the database
 * 
 *  Revision 1.26  2004/04/09 21:33:53  perev
 *  Cleanup. destructor of maker more deleting
 * 
 *  Revision 1.25  2004/04/08 21:35:12  perev
 *  Leak off
 * 
 *  Revision 1.24  2003/10/01 00:43:16  pavlinov
 *  Change searching order for Geant hits
 * 
 *  Revision 1.23  2003/09/30 01:28:49  jeromel
 *  Undo correction until logic reshape
 * 
 *  Revision 1.22  2003/09/28 03:06:01  jeromel
 *  restored leak_assign (logic needs to be modified to get rid of it)
 * 
 *  Revision 1.21  2003/09/28 01:57:55  jeromel
 *  LEAK_SCOPE and LEAK_ASSIGN removed
 * 
 *  Revision 1.20  2003/09/23 15:19:52  suaide
 *  fixed bugs and modifications for embedding
 * 
 *  Revision 1.18  2003/09/02 17:58:00  perev
 *  gcc 3.2 updates + WarnOff
 * 
 *  Revision 1.17  2003/04/30 20:36:47  perev
 *  Warnings cleanup. Modified lines marked VP
 * 
 *  Revision 1.16  2003/01/23 03:09:02  jeromel
 *  Include modif
 * 
 *  Revision 1.15  2003/01/17 21:21:28  suaide
 *  small bug fixed to compile on Solaris
 * 
 *  Revision 1.14  2003/01/17 00:44:20  suaide
 *  Added new EMC database scheme
 * 
 *  Revision 1.13  2002/09/17 18:37:01  pavlinov
 *  mDbMaker was zero
 * 
 *  Revision 1.12  2002/09/16 22:14:50  pavlinov
 *  No DB for EMC before 24-09-2001
 * 
 *  Revision 1.11  2002/09/10 16:51:32  pavlinov
 *  Discard line with mDbMaker->SetDateTime
 * 
 *  Revision 1.10  2002/06/04 16:09:36  pavlinov
 *  added option with DB(pedestal ans calibration  coefficients
 * 
 *  Revision 1.9  2002/06/03 23:35:10  pavlinov
 *  Last correction without DB for ped and calib. coeff.
 * 
 *  Revision 1.8  2002/05/30 17:35:06  pavlinov
 *  changed the way of searching of GEANT data
 * 
 *  Revision 1.7  2001/09/22 00:29:42  pavlinov
 *  No public constructor for StEmcGeom
 * 
 *  Revision 1.6  2001/05/14 01:21:45  pavlinov
 *  In method StMcEmcHitCollection::module(m) m is module number, not index
 * 
 *  Revision 1.5  2001/03/23 19:02:51  pavlinov
 *  Get pointer to chain via list of browsables
 * 
 *  Revision 1.4  2001/03/22 22:04:38  pavlinov
 *  Clean up for mdc4
 * 
 *  Revision 1.3  2001/03/15 17:21:32  pavlinov
 *  Fixed error for module=1
 * 
 *  Revision 1.2  2001/02/02 23:59:59  pavlinov
 *  New function Browse() and cleanup for new version of BFC
 * 
 *  Revision 1.1  2000/10/23 22:53:14  pavlinov
 *  First working C++ version
 *****************************************************************************/
