// $Id: StEmcSimulatorMaker.cxx,v 1.60 2015/03/13 01:02:05 perev Exp $

#include "StEmcSimulatorMaker.h"

#include <assert.h>
#include <set>

#include "StMcEvent/StMcCalorimeterHit.hh"
#include "StMcEvent/StMcEmcModuleHitCollection.hh"
#include "StMcEvent/StMcEmcHitCollection.hh"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcTrack.hh"

#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEvent.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"

#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"

#include "StEmcSimpleSimulator.h"
#include "StEmcPmtSimulator.h"

ClassImp(StEmcSimulatorMaker)

StEmcSimulatorMaker::StEmcSimulatorMaker(const char *name):StMaker(name) {
    // checking if this is embedding mode by looking for StEmcRawMaker or StEmcADCtoEMaker in this chain
    mEmbeddingMode = (GetMaker("emcRaw") || GetMaker("Eread"));
    LOG_INFO <<"StEmcSimulatorMaker EMBEDDING mode = "<< (Int_t)mEmbeddingMode <<endm;
    
    // initialize control table
    for(int i=0; i<MAXDETBARREL; i++) { 
        mMakeFullDetector[i]    = false;
        mCheckStatus[i]         = true;
        mDoZeroSuppression[i]   = (mEmbeddingMode) ? false:true;
        mPedestalCut[i]         = 1.5;
        mCalibOffset[i]         = 0.0;
        mCalibSpread[i]         = 0.0;
        mMaxAdcSpread[i]        = 0.0;
        mCrossTalk[i]           = 0.0;
    }
    
    mMaxAdc[0]  = 4095;
    mMaxAdc[1]  = 1023;
    mMaxAdc[2]  = 1023;
    mMaxAdc[3]  = 1023;
    
    // adc2e doesn't suppress BTOW hits, so we won't either
    mDoZeroSuppression[0] = false;

    // trigger emulation expects full peds for BTOW
    mMakeFullDetector[0] = true;

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
    mPosition = new StEmcPosition();
    
    // simulators are instantiated in Init, but we set some params here in case the user wants to change them
    mSimulatorMode[BTOW-1]  = StEmcVirtualSimulator::kPrimarySecondaryFullMode;
    mSimulatorMode[BPRS-1]  = StEmcVirtualSimulator::kPrimarySecondaryFullMode;
    mSimulatorMode[BSMDE-1] = StEmcVirtualSimulator::kSimpleMode;
    mSimulatorMode[BSMDP-1] = StEmcVirtualSimulator::kSimpleMode;
    
    mIsBFC = GetParentChain()->InheritsFrom("StBFChain");
    LOG_INFO << "Is StEmcSimulatorMaker in BFC? " << mIsBFC << endm;
}

StEmcSimulatorMaker::~StEmcSimulatorMaker() {
#if 0
    delete mSimulator[BTOW-1];
    delete mSimulator[BPRS-1];
    delete mSimulator[BSMDE-1];
    delete mSimulator[BSMDP-1];
    
    delete mTables;
    delete mPosition;
#endif
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
        mSimulator[det-1]->setMaximumAdc(mMaxAdc[det-1]);
        mSimulator[det-1]->setMaximumAdcSpread(mMaxAdcSpread[det-1]);
    }
    
    return kStOk;
}

void StEmcSimulatorMaker::Clear(const char*) {
    mMcEvent = NULL;
}

Int_t StEmcSimulatorMaker::Make() {
    mMcEvent = dynamic_cast<StMcEvent*>(GetDataSet("StMcEvent"));
    if(!mMcEvent) {
        LOG_ERROR << "couldn't find StMcEvent for this event" << endm;
        return kStErr;
    }
    
    mEmcMcHits[0] = mMcEvent->bemcHitCollection();
    mEmcMcHits[1] = mMcEvent->bprsHitCollection();
    mEmcMcHits[2] = mMcEvent->bsmdeHitCollection();
    mEmcMcHits[3] = mMcEvent->bsmdpHitCollection();
        
    // simulation of optical cross-talk in BSMDE by M.Betancourt 11/2007
    vector<StMcTrack*> McTracks = mMcEvent->tracks();
    for(unsigned int i = 0; i < McTracks.size(); ++i) {
        StMcTrack *track = McTracks.at(i);
        if(track->stopVertex()) continue; // Existing stopVertex means intermediate particle
        makeCrossTalk(track);
    }
    
    // simulate pedestals where no hit was found if makeFullDetector is specified
    int maxChannels[4] = {4800, 4800, 18000, 18000};
    std::vector<int> hasHit;
    std::vector<int>::const_iterator iter;
    
    int softId, module, eta, sub;
    for(int det=BTOW; det<=BSMDP; det++) {
        if(mMakeFullDetector[det-1] == 0) continue;
        
        // identify the channels that already have real hits        
        for(unsigned int mod=1; mod<=mEmcMcHits[det-1]->numberOfModules(); mod++) {
            const StMcEmcModuleHitCollection* module = mEmcMcHits[det-1]->module(mod);
            const vector<StMcCalorimeterHit*> hits   = module->detectorHits();
            for(unsigned long i=0; i<hits.size(); i++) {
                mGeom[det-1]->getId(hits[i]->module(), hits[i]->eta(), hits[i]->sub(), softId);
                hasHit.push_back(softId);
            }
        }
        
        // sort the vector for constant-time access while looping through all channels
        std::sort(hasHit.begin(), hasHit.end());
        iter = hasHit.begin();
        int nextHitId = 0;
        if(hasHit.size()) nextHitId = *iter;
        
        StMcCalorimeterHit *mcHit = new StMcCalorimeterHit();
        
        // add hits with 0 energy to channels that don't have real hits
        for(int softId=1; softId<=maxChannels[det-1]; softId++) {
            if( softId != nextHitId ) {
                mGeom[det-1]->getBin(softId,module,eta,sub);
                mcHit->setModule(module);
                mcHit->setEta(eta);
                mcHit->setSub(sub);
                mcHit->setdE(0.0);
                mcHit->setParentTrack(NULL);
                
                // push_back manually instead of using addHit to save (lots of) time
                mEmcMcHits[det-1]->module(module)->detectorHits().push_back(mcHit);
                mcHit = new StMcCalorimeterHit();
            }
            else {
                iter++;
                if(iter != hasHit.end()) nextHitId = *iter;
            }
        }
        
        delete mcHit;
        hasHit.clear();
    }
    
    // lots of LOG_DEBUG statements
    for(int det=BTOW; det<=BSMDP; det++) {
        LOG_DEBUG << *(mEmcMcHits[det-1]) << endm;
        for(unsigned int mod=1; mod<=mEmcMcHits[det-1]->numberOfModules(); mod++) {
            const StMcEmcModuleHitCollection *module = mEmcMcHits[det-1]->module(mod);
            const vector<StMcCalorimeterHit*> hits = module->detectorHits();
            for(unsigned long i=0; i<hits.size(); i++) {
                int softId; mGeom[det-1]->getId(hits[i]->module(), hits[i]->eta(), hits[i]->sub(), softId);
                LOG_DEBUG << "softId:" << softId << " mod:" << hits[i]->module() << " eta:" << hits[i]->eta() << " sub:" << hits[i]->sub() << " dE:" << hits[i]->dE() << endm;
            }
        }
    }
    
    // now convert the energy depositions to ADC values
    makeRawHits();

    return StMaker::Make();
}

// translate each StMcCalorimeterHit into an StEmcRawHit and save in StEmcCollection
void StEmcSimulatorMaker::makeRawHits() {
    mTables->loadTables(this);
    
    // get a valid StEmcCollection to store the hits
    if (mEmbeddingMode) {
        mEmcCollection = new StEmcCollection();
    } else {
        StEvent* event = (StEvent*)GetInputDS("StEvent");
        if (!event) {
            event = new StEvent();
            AddData(event);
        }
        mEmcCollection = event->emcCollection();
        if (!mEmcCollection) {
            mEmcCollection = new StEmcCollection();
            event->setEmcCollection(mEmcCollection);
        }
        StMuDst* mudst = (StMuDst*)GetInputDS("MuDst");
        if (mudst) {
            mudst->setEmcCollection(mEmcCollection);
        }
    }
    
    for (int det = BTOW;det <= BSMDP;det++) {
        StDetectorId detectorId = kUnknownId;
        switch(det) {
            case BTOW:  detectorId = kBarrelEmcTowerId; break;
            case BPRS:  detectorId = kBarrelEmcPreShowerId; break;
            case BSMDE: detectorId = kBarrelSmdEtaStripId; break;
            case BSMDP: detectorId = kBarrelSmdPhiStripId; break;
        }
        StEmcDetector *detector = new StEmcDetector(detectorId, 120); // should this be a magic number?
        mEmcCollection->setDetector(detector);
        
        for (unsigned int mod = 1;mod <= mEmcMcHits[det-1]->numberOfModules();mod++) {
            const StMcEmcModuleHitCollection* module = mEmcMcHits[det-1]->module(mod);
            const vector<StMcCalorimeterHit*> hits = module->detectorHits();
            for (unsigned long i = 0;i < hits.size();i++) {
                int softId;
		mGeom[det-1]->getId(hits[i]->module(), hits[i]->eta(), hits[i]->sub(), softId);
                LOG_DEBUG << "-----------------------------------------------------------------------------------------------" << endm;
                // check hit status
                if (mCheckStatus[det-1]) { 
                    if (mTables->status(det, softId) != 1) { 
                        LOG_DEBUG << Form("det=%2d  softId=%5d -- removing hit b/c DB status!=1", detectorId, softId) << endm;
                        continue;
                    }
                }
                
                StEmcRawHit *rawHit = mSimulator[det-1]->makeRawHit(hits[i]);
                rawHit->setCalibrationType(0);
                
                // do zero suppression
                if (mDoZeroSuppression[det-1]) { 
                    float pedMean = mTables->pedestal(det, softId);
                    float pedRMS  = mTables->pedestalRMS(det, softId);
                    if ((rawHit->adc() - pedMean) < (mPedestalCut[det-1] * pedRMS)) {
                        LOG_DEBUG << "removing hit b/c it failed pedestal cut" << endm;
                        delete rawHit;
                        continue;
                    }
                }
                
                // store GEANT dE in MuDST if this is BFC
                if (mIsBFC && !mEmbeddingMode) {
                    rawHit->setEnergy(hits[i]->dE());
                }
                
                detector->addHit(rawHit);
            } // loop over hits
        } // loop over modules
    } // loop over detectors
}

////////////////////////////////////////////////////////////
//             Leak energy deposited by track             //
//                per optical cross talk                  //
////////////////////////////////////////////////////////////
void StEmcSimulatorMaker::makeCrossTalk(StMcTrack *track)
{
    // Loop over BEMC detectors
    for (int det = BSMDE;det <= BSMDP;++det) {
        // No cross talk for the phi strips (not sure how to simulate yet) 
        if(det == BSMDP) continue;
        // Fetch calorimeter hits deposited by the StMcTrack
        vector<StMcCalorimeterHit*> trackHits;
        switch(det) {
	    case BSMDE: trackHits = track->bsmdeHits(); break;
            case BSMDP: trackHits = track->bsmdpHits(); break;
        }
        LOG_DEBUG << " this track has BSMD of size = " << trackHits.size() << endm;
		
        ////////////////////////////////////////////////////////////////////////
        //    Find the detector element with the largest energy deposition    //
        ////////////////////////////////////////////////////////////////////////
        double highEnergy = -1;
        int highElement = -1;
        for (unsigned long j = 0;j < trackHits.size();++j) {
            double energy = trackHits.at(j)->dE();				
            if ((energy > highEnergy) || (highEnergy < 0)) {
                highEnergy = energy;
                highElement = j;
            }
        }
        if (highElement != -1) {
            ///////////////////////////////////////////////////////////////////////////////////
            //    Calculate the software ID of the high element and its nearest neighbors    //
            ///////////////////////////////////////////////////////////////////////////////////
            StMcCalorimeterHit* highHit = trackHits.at(highElement);
            
	    const Int_t numCross = 5;
            int softIds[2 * numCross + 1] = {0};
            // Pointers to the relevant detector hits
	    // Will be assigned to point to the strip in the next step.
            StMcCalorimeterHit *detectorNextHits[2 * numCross + 1] = {0};

            switch (det) {
		//1=bemc, 2=bprs, 3=bsmde, 4=bsmdp
                case BSMDE:
                case BSMDP:
		    for (Int_t i = 0;i < (2*numCross + 1);i++) {
                	softIds[i] = mPosition->getNextId(det, highHit->module(), highHit->eta(), highHit->sub(), (det == BSMDE) ? (i - numCross) : 0, (det == BSMDP) ? (i - numCross) : 0);
		    }
		    break;
            }

            int modHigh = 0;
            float etaHigh = 0;
            mGeom[det - 1]->getId(highHit->module(), highHit->eta(), highHit->sub(), softIds[numCross]);
            modHigh = highHit->module();
            mGeom[det - 1]->getEta(softIds[numCross], etaHigh);
            
            ///////////////////////////////////////////////////////////////////////////////////////
            //    Find the calorimeter hits corresponding to the high strip and its neighbors    //
            ///////////////////////////////////////////////////////////////////////////////////////
    	    double totalenergyb = 0; // total energy before cross talk, part of the energy conservation check
	    double totalenergya = 0; // total energy after cross talk
            for (unsigned int mod = 1;mod <= mEmcMcHits[det- 1]->numberOfModules();++mod) {
                const StMcEmcModuleHitCollection* module = mEmcMcHits[det - 1]->module(mod);
                const vector<StMcCalorimeterHit*> &detectorHits = module->detectorHits();
                for (unsigned long k = 0;k < detectorHits.size();++k) {
                    int softTemp; 
                    mGeom[det - 1]->getId(detectorHits[k]->module(), detectorHits[k]->eta(), detectorHits[k]->sub(), softTemp);
                    // loop through the strips in this module and assign the pointer to the strip objects
		    for (Int_t i = 0;i < (2*numCross + 1);i++) {
                	if (softTemp == softIds[i]) {
                    	    detectorNextHits[i] = detectorHits[k]; 
			    totalenergyb += detectorHits[k]->dE();
                	}
		    }
                } // detectorHits
	    } // Modules
			
    	    // cross-talk falls off linearly as we approach edge of barrel
	    const float crossTalk = mCrossTalk[det - 1] * (1 - fabs(etaHigh) );

	    double leakE1 = 0, leakE2 = 0;
	    StMcCalorimeterHit *tempHit = new StMcCalorimeterHit();
	    for (int i = 0;i <= numCross;i++) {
		double energy1 = detectorNextHits[numCross + i] ? detectorNextHits[numCross + i]->dE() : 0;
		double energy2 = detectorNextHits[numCross - i] ? detectorNextHits[numCross - i]->dE() : 0;
		{LOG_DEBUG << "In: softId1 = " << softIds[numCross + i] << ", softId2 = " << softIds[numCross - i] << endm;}
		{LOG_DEBUG << "In: energy1 = " << energy1 << ", energy2 = " << energy2 << endm;}
		{LOG_DEBUG << "In: leakE1 = " << leakE1 << ", leakE2 = " << leakE2 << endm;}
		//if (i == 0) {LOG_DEBUG << "old high Energy = " << energy1 << endm;}
		energy1 += leakE1;
		energy2 += leakE2;
		// there are 3 situations: 
		// 1: From the geometry, there is no next next strip (softNextNext==0)
		// 2: There should be next next strip, but no pointer/hit to it, meaning it has zero energy (detectorNextNext==NULL)
		// 3: There is next next strip and a pointer/hit with energy
		// wenqin
		if ((i < numCross) && softIds[numCross + i] && softIds[numCross + (i + 1)]) {
		    // situation2 or situation3, leak the difference
    		    const double energyNext1 = detectorNextHits[numCross + (i + 1)] ? detectorNextHits[numCross + (i + 1)]->dE() : 0;
		    //if (i == 0) {LOG_DEBUG << "old next Energy = " << energyNext1 << endm;}
		    leakE1 = crossTalk * (energy1 - energyNext1);
		} else {
		    leakE1 = 0; //situation1: do nothing
		}
		//if (i == 0) {LOG_DEBUG << "central strip leakage " << leakE1 << endm;}
		if ((i < numCross) && softIds[numCross - i] && softIds[numCross - (i + 1)]) {
		    // situation2 or situation3, leak the difference
    		    const double energyNext2 = detectorNextHits[numCross - (i + 1)] ? detectorNextHits[numCross - (i + 1)]->dE() : 0;
		    //if (i == 0) {LOG_DEBUG << "old previous Energy = " << energyNext2 << endm;}
		    leakE2 = crossTalk * (energy2 - energyNext2);
		} else {
		    leakE2 = 0; //situation1: do nothing
		}
		//if (i == 0) {LOG_DEBUG << "central strip forward leakage " << leakE2 << endm;}
		energy1 -= leakE1;
		if (i == 0) {
		    energy1 -= leakE2;
		} else {
		    energy2 -= leakE2;
		}
		{LOG_DEBUG << "Out: energy1 = " << energy1 << ", energy2 = " << energy2 << endm;}
		{LOG_DEBUG << "Out: leakE1 = " << leakE1 << ", leakE2 = " << leakE2 << endm;}
		//if (i == 0) {LOG_DEBUG << "new high Energy after leaking is " << energy1 << endm;}
		int module, eta, sub;
		if (detectorNextHits[numCross + i]) {
		    detectorNextHits[numCross + i]->setdE(energy1);
		} else {
		    if (softIds[numCross + i]) {
	    		///////////////////////////////////////////////////////////////////////////
		        // Create empty calorimeter hits for  neighbors with no energy deposited,// 
			// i.e. energy is zero! excluding neighbors in different modules         //
		        ///////////////////////////////////////////////////////////////////////////
    			mGeom[det - 1]->getBin(softIds[numCross + i], module, eta, sub);
			tempHit->setModule(module);
			tempHit->setEta(eta);
			tempHit->setSub(sub);
		        tempHit->setParentTrack(NULL);
			tempHit->setdE(energy1);
			detectorNextHits[numCross + i] = tempHit;
			StMcEmcHitCollection::EAddHit returnCode = mEmcMcHits[det - 1]->addHit(tempHit);
		        if (returnCode == StMcEmcHitCollection::kNew) {
			    tempHit = new StMcCalorimeterHit();
			}
		    }
		}
		if (i != 0) {
		    if (detectorNextHits[numCross - i]) {
			detectorNextHits[numCross - i]->setdE(energy2);
		    } else {
			if (softIds[numCross - i]) {
			    mGeom[det - 1]->getBin(softIds[numCross - i], module, eta, sub);
		    	    tempHit->setModule(module);
			    tempHit->setEta(eta);
		            tempHit->setSub(sub);
		    	    tempHit->setParentTrack(NULL);
		    	    tempHit->setdE(energy2);
		    	    detectorNextHits[numCross - i] = tempHit;
		    	    StMcEmcHitCollection::EAddHit returnCode = mEmcMcHits[det - 1]->addHit(tempHit);
		    	    if (returnCode == StMcEmcHitCollection::kNew) {
		    		tempHit = new StMcCalorimeterHit();
			    }
			}
		    }
		}
		totalenergya += detectorNextHits[numCross + i] ? detectorNextHits[numCross + i]->dE() : 0;
		if (i != 0) totalenergya += detectorNextHits[numCross - i] ? detectorNextHits[numCross - i]->dE() : 0;
	    }
	    {LOG_DEBUG << "total energy before is " << totalenergyb << " and total energy after immediately, is " << totalenergya << endm;}
	    delete tempHit;
        } // highElement 
    } // det Loop
}

/*****************************************************************************
 *  $Log: StEmcSimulatorMaker.cxx,v $
 *  Revision 1.60  2015/03/13 01:02:05  perev
 *  remove delete which crashed BFC
 *
 *  Revision 1.59  2010/10/14 21:18:57  ogrebeny
 *  Changes from Wenqin (wqxu@ucla.edu):
 *
 *  For the cross talk part, make the energy assignments of the newly created
 *  strips happen before they are added to the mEmcMcHits collection, otherwise
 *  the energy assignments won't be recorded in the collection.
 *
 *  The cross talk leakage is now proportional to the difference of energies of two neighbor
 *  strips. The leakage range is extended to +/- 5 strips.
 *
 *  The cross talk in the BSMD-Phi still needs more solid adjustment, disabled for now.
 *
 *  Revision 1.58  2008/11/17 21:44:24  kocolosk
 *  don't store GEANT dE in MuDST if we're embedding
 *
 *  Revision 1.56  2008/01/24 15:22:36  kocolosk
 *  set MuDst's StEmcCollection pointer
 *
 *  Revision 1.55  2007/12/12 23:29:47  kocolosk
 *  full pedestal simulation is now default for BTOW
 *
 *  Revision 1.54  2007/12/12 22:12:47  kocolosk
 *  push_back detector hits manually instead of using addHit to save (lots of) time
 *
 *  Revision 1.53  2007/11/28 16:18:58  kocolosk
 *  optical cross-talk simulation by Mike Betancourt
 *  http://www.star.bnl.gov/HyperNews-star/protected/get/phana/144.html
 *
 *  Revision 1.52  2007/10/08 15:28:38  kocolosk
 *  setMaximumAdc(Spread) methods allow for better simulation of BSMD ADC response
 *  http://www.star.bnl.gov/HyperNews-star/get/emc2/2507.html
 *
 *  Revision 1.51  2007/09/21 13:16:20  kocolosk
 *  bugfix with Martijn's help.  There is currently an important difference
 *  between the way StMcEvent fills its collections and the way the old simulator
 *  filled them.  This fix ensures at most one MC hit per detector element (tower/strip)
 *
 *  Revision 1.50  2007/09/15 18:36:35  kocolosk
 *  changed defaults so makeFullDetector is false and so zero suppression is turned off for BTOW
 *
 *  Revision 1.49  2007/09/12 13:31:45  kocolosk
 *  two small changes to suppress compiler warnings
 *
 *  Revision 1.48  2007/09/12 03:06:11  kocolosk
 *  embedding mode also set if StEmcADCtoEMaker is in chain (non-bfc embedding)
 *
 *  Revision 1.47  2007/09/12 02:58:53  kocolosk
 *  look for emcRaw, not emcEmbed, to determine if it's an embedding chain
 *
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
 
