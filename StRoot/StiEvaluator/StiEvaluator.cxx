//StiEvaluator.cxx
// A. Rose (WSU)
//8/01

//ROOT
#include "TFile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TClonesArray.h"

//STD
#include <stdexcept>
#include <math.h>
#include <iostream.h>
using namespace std;

//StEvent
#include "StEventTypes.h"

//StMcEvent
#include "StMcEventTypes.hh"

//Association
#include "StAssociationMaker/StTrackPairInfo.hh"

//Sti includes
#include "Sti/StiTrackContainer.h"
#include "Sti/StiEvaluableTrack.h"
#include "Sti/StiTrack.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiHit.h"
#include "Sti/StiKalmanTrackNode.h"

//StiEvaluator includes
#include "StiEventAssociator.h"
#include "StiTrackPairInfo.h"
#include "StiEvaluator.h"

StiEvaluator* StiEvaluator::sinstance = 0;

static unsigned int commonHitCut=5;

StiEvaluator::StiEvaluator(const string& fname)
    : mFileName(fname), mFile(0), mTree(0), mEntry(0)
{
    cout <<"StiEvaluator::StiEvaluator()"<<endl;
    if (mFileName=="empty") {
	cout <<"StiEvaluator::StiEvaluator() ERROR:\t";
	cout <<"No file name specified for output file.";
	cout <<"Abort witout building!"<<endl;
    }
    else {
	build();
    }
    sinstance = this;
}

StiEvaluator::~StiEvaluator()
{
    cout <<"StiEvaluator::~StiEvaluator()"<<endl;
    mFile->Write();
    mFile->Close();
}

StiEvaluator* StiEvaluator::instance(const string val)
{
    return (sinstance) ? sinstance : new StiEvaluator(val);
}

void StiEvaluator::kill()
{
    if (sinstance) {
	delete sinstance;
	sinstance = 0;
    }
}

void StiEvaluator::build()
{
    cout <<"StiEvaluator::build()"<<endl;
    
    //Must open TFile first if you want ntuple to disk
    cout <<"Opening ROOT file: "<<mFileName<<endl;
    mFile = new TFile(mFileName.c_str(),"RECREATE");

    mEntry = new TrackEntry();
    mTree = new TTree("TestTree","The Test Tree");

    Int_t buffsize = 64000;
    Int_t splitlevel = 1;
    mTree->Branch("TestBranch","TrackEntry",&mEntry, buffsize, splitlevel);

    cout <<"\tdone"<<endl;
    
}

void StiEvaluator::evaluate(const StiTrackContainer* trackStore)
{
    cout <<"\nStiEvaluator::evaluate() - INFO - Beginning event evaluation"<<endl;
    cout <<"\tNumber of StiTracks:\t"<<trackStore->size()<<endl;

    typedef StiEventAssociator::McToInfoPairMap McMap;
    typedef StiEventAssociator::InfoPair StiInfoPair;

    McMap& myMap = StiEventAssociator::instance()->mcToInfoPairMap();
    cout <<"\tNumber of StMcTracks:\t"<<myMap.size()<<endl;

    unsigned int iMcTrack=0;
    //cout <<"\tLoop on McToInfoPairMap"<<endl;
    for (McMap::iterator outer_it=myMap.begin(); outer_it!=myMap.end(); ++outer_it) {

	if (fmod(static_cast<double>(iMcTrack++),100)==0) {
	    cout <<"\tChugging on track:\t"<<iMcTrack<<endl;
	}
	
	mEntry->clear();
	StMcTrack* mcTrack = (*outer_it).first;
	
	//cout <<"-- New McTrack: "<<mcTrack<<endl;
	
	StiTrackPairInfo& testInfo = (*outer_it).second.second;
	if ( testInfo.partnerMcTrack()==0 ) {// This McTrack was not found!
	    //Fill McTrack info:
		//cout <<"Track not found, fill McTrack and Return"<<endl;
	    mEntry->clear();
	    mEntry->setMcTrack(mcTrack, 0, false); //Dont incremnt the nFound counter
	    mTree->Fill();
	}

	else {
		//cout <<"Track found, find best match and fill"<<endl;
	    //we have to choose the best ITTF trackf or this Mc Track (best common hits)
	    //Start kludge here (should be an algorithm call)
	    pair< McMap::iterator, McMap::iterator > range = myMap.equal_range(mcTrack);
	    
	    StiTrackPairInfo* bestStiPair=0;
	    StTrackPairInfo* bestGlobalPair=0;
	    unsigned int mostCommon=0;

	    unsigned int nTimes=0;
	    for (McMap::iterator it=range.first; it!=range.second; ++it) {
		++nTimes;
		
		StiTrackPairInfo& info = (*it).second.second;
		
		if (info.commonTpcHits()>mostCommon && info.commonTpcHits()>commonHitCut) { //update, remember
		    mostCommon = info.commonTpcHits();
		    bestGlobalPair = (*it).second.first;
		    bestStiPair = &info;
		}
	    }
	    //cout <<"\t Found this MC Track: "<<nTimes<<" times"<<endl;
	    //End kludge

	    //Fill for best match
	    if (bestStiPair!=0) { //Finally, we can fill!
		
		//Fill McTrack info:
		mEntry->clear();
		mEntry->setMcTrack(bestStiPair->partnerMcTrack(), 1, true);

		//Check to see if Global assoc worked!
		if (bestGlobalPair!=0) {
		    mEntry->setGlobalTrack(bestGlobalPair->partnerTrack());
		    mEntry->setGlobalAssoc(bestGlobalPair);
		}
		//We already know that we're safe here
		mEntry->setStiTrack(bestStiPair->partnerTrack());
		mEntry->setAssociation(*bestStiPair);

		const StiKalmanTrack* tkt =
		    dynamic_cast<const StiKalmanTrack*>(bestStiPair->partnerTrack());
		if (!tkt) {
		    cout <<"StiEvaluator::evaluateForEvent(). ERROR:\t"
			 <<"cast to kalman track failed."<<endl;
		}
		else {
		    //fillHitEntry(tkt);
		}
		mTree->Fill();
	    }

	    //Fill for all other matches (split tracks!!!)
	    for (McMap::iterator it=range.first; it!=range.second; ++it) {
		StTrackPairInfo* globalPair = (*it).second.first;
		StiTrackPairInfo& info = (*it).second.second;
		if (&info!=bestStiPair && info.commonTpcHits()>commonHitCut) { //don't repeat for bestMatch
		    //Fill McTrack info:
		    mEntry->clear();
		    mEntry->setMcTrack(bestStiPair->partnerMcTrack(), 1, false);
		    if (globalPair!=0) {
			mEntry->setGlobalTrack(globalPair->partnerTrack());
			mEntry->setGlobalAssoc(globalPair);
		    }
		    
		    //We already know that we're safe here
		    mEntry->setStiTrack(info.partnerTrack());
		    const StiKalmanTrack* tkt =
			dynamic_cast<const StiKalmanTrack*>(info.partnerTrack());
		    if (!tkt) {
			cout <<"StiEvaluator::evaluateForEvent(). ERROR:\t"
			     <<"cast to kalman track failed."<<endl;
		    }
		    else {
			//fillHitEntry(tkt);
		    }
		    mEntry->setAssociation(info);
		    mTree->Fill();
		}
		
	    }
	}
	
    }
    cout <<"StiEvaluator::evaluate() - INFO - Completed"<<endl;
}

void StiEvaluator::fillHitEntry(const StiKalmanTrackNode* node)
{
    //Reset the entry:
    mStiHitEntry.reset();
    
    //Fill node-wise quantities:
    mStiHitEntry.nodeAlpha = node->fAlpha;
    mStiHitEntry.nodeLocalX = node->fX;
    mStiHitEntry.nodeLocalY = node->fP0;
    mStiHitEntry.nodeLocalZ = node->fP1;
    mStiHitEntry.nodeLocalEta = node->fP2;
    mStiHitEntry.nodeLocalCurvature = node->fP3;
    mStiHitEntry.nodeLocalTanLambda = node->fP4;
    mStiHitEntry.nodeLocalChi2 = node->fChi2;    
    
    //Fill hit-wise quantities, if there's a hit for this node:
    const StiHit* hit = node->getHit();
    if (hit) {
	fillHitEntry(hit);
	mStiHitEntry.nodeHasHit = 1;
    }

    //Add to the track entry
    mEntry->addStiHitEntry(mStiHitEntry);

}

void StiEvaluator::fillHitEntry(const StiHit* hit)
{
    mStiHitEntry.hitTimesUsed = hit->timesUsed();
    
    mStiHitEntry.hitPosition = hit->position();
    mStiHitEntry.hitRefAngle = hit->refangle();
    mStiHitEntry.hitLocalX = hit->x();
    mStiHitEntry.hitLocalY = hit->y();
    mStiHitEntry.hitLocalZ = hit->z();

    mStiHitEntry.hitLocalSxx = hit->sxx();
    mStiHitEntry.hitLocalSyy = hit->syy();
    mStiHitEntry.hitLocalSzz = hit->szz();
    
    mStiHitEntry.hitLocalSxy = hit->sxy();
    mStiHitEntry.hitLocalSxz = hit->sxz();
    mStiHitEntry.hitLocalSyz = hit->syz();
    
    const StThreeVectorF& pos = hit->globalPosition();
    mStiHitEntry.hitGlobalX = pos.x();
    mStiHitEntry.hitGlobalY = pos.y();
    mStiHitEntry.hitGlobalZ = pos.z();
    /*
      cout <<"\tGlobal Pos:\t"
      <<mStiHitEntry.hitGlobalX<<"\t"
      <<mStiHitEntry.hitGlobalY<<"\t"
      <<mStiHitEntry.hitGlobalZ<<endl;
    */
}

void StiEvaluator::fillHitEntry(const StiKalmanTrack* track)
{
    //Start at the last node on the track, work upwards until you find the root of the tree:
    
    StiKalmanTrackNode* node = track->getLastNode(); //start at innermost
    unsigned int nodes=0;
    bool go=true;
    while (go) {
	++nodes;
	fillHitEntry(node);
	//now check for parent:
	if (node->isRoot()) { //this means that it's the root, no where else to go
	    go=false;
	}
	else {
	    node = dynamic_cast<StiKalmanTrackNode*>(node->getParent());
	    if (!node) {
		cout <<"StiEvaluator::fillHitEntry(const StiKalmanTrack&) ERROR:\t"
		     <<"Cast to StiKalmanTrackNodeFailed.  Abort"<<endl;
		return;
	    }
	}
    }
    // cout <<"StiEvaluator::fillHitEntry(StiKalmanTrack)\t"
    // <<nodes<<" nodes processed for track"<<endl;
}


//Temp, to be moved to own file

