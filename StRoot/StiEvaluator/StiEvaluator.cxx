//StiEvaluator.cxx
// A. Rose (WSU)
//8/01

//ROOT
#include "TFile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TClonesArray.h"

//STD
#include <iostream.h>

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

//StiEvaluator includes
#include "StiEvaluator.h"

StiEvaluator* StiEvaluator::sinstance = 0;

StiEvaluator::StiEvaluator() : mFile(0), mTree(0), mEntry(0)
{
    cout <<"StiEvaluator::StiEvaluator()"<<endl;
    build();
    sinstance = this;
}

StiEvaluator::~StiEvaluator()
{
    cout <<"StiEvaluator::~StiEvaluator()"<<endl;
    mFile->Write();
    mFile->Close();
}

StiEvaluator* StiEvaluator::instance()
{
    return (sinstance) ? sinstance : new StiEvaluator();
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
    cout <<"StiEvaluator::build().\tOpening Root file, building TTree(s)"<<endl;
    //Must open TFile first if you want ntuple to disk
    mFile = new TFile("TestEvaluation.root","RECREATE");

    mEntry = new TrackEntry();
    mTree = new TTree("TestTree","The Test Tree");

    Int_t buffsize = 64000;
    Int_t splitlevel = 1;
    mTree->Branch("TestBranch","TrackEntry",&mEntry, buffsize, splitlevel);

    cout <<"\tdone"<<endl;
    
}

void StiEvaluator::evaluateForEvent(const StiTrackContainer* trackStore)
{
    cout <<"\nStiEvaluator::evaluateForEvent()"<<endl;
    cout <<"\tNumber of StiTracks:\t"<<trackStore->size()<<endl;

    for (StiTrackContainer::stitrackvec::const_iterator it=trackStore->begin(); it!=trackStore->end(); ++it) {
	StiTrack* temp = (*it);
	//Now you've got the track, do what you want with it
	StiEvaluableTrack* track = dynamic_cast<StiEvaluableTrack*>(temp);
	if (!track) {
	    cout <<"StiEvaluator::evaluateForEvent(). ERROR!\tCast to Evaluable track failed.  ABORT"<<endl;
	    return;
	}

	//Now we have an StiEvaluableTrack
	StTrackPairInfo* associatedPair = track->stTrackPairInfo();
	if (!associatedPair) {
	    cout <<"StiEvaluator::evaluateForEvent(). ERROR!\t.Associated Pair==0.  Abort"<<endl;
	    return;
	}
	//Call some function to actually fill TTree object(s)
	mEntry->setMcTrack(associatedPair->partnerMcTrack());
	mEntry->setGlobalTrack(associatedPair->partnerTrack());
	//mEntry->setStiTrack(track);
	mTree->Fill();

    }
}

//Temp, to be moved to own file



ClassImp(TrackEntry)
    
TrackEntry::TrackEntry() 
{
}

void TrackEntry::clear()
{
    mcTrackId = mcTrackPsi = mcTrackPt = mcTrackChi2 = 0.;
    globalTrackQ = 0;
    globalTrackM = globalTrackPt = globalTrackPsi = globalTrackChi2 = globalTrackNHit = 0.;
    stiTrackM = stiTrackPt = stiTrackPsi = stiTrackChi2 = stiTrackNHit = 0.;
}

/*void TrackEntry::addArrayEntry(const ArrayEntry& val)
{
    TClonesArray& cArr = *mArray;
    new(cArr[mCounter++]) ArrayEntry(val);
    }
*/

double TrackEntry::getMcTrackId()
{
  return mcTrackId;
}

double TrackEntry::getMcTrackPt()
{
  return mcTrackPt;
}

void TrackEntry::setGlobalTrack(StTrack *newtrack)
{
  const StThreeVectorF& mom = newtrack->geometry()->momentum();
  globalTrackPt = mom.perp();
  globalTrackQ  = newtrack->geometry()->charge();
  globalTrackPsi = newtrack->geometry()->psi();
}

void TrackEntry::setStiTrack(StiTrack *newtrack)
{
    /*
    const StThreeVectorD& mom = newtrack->geometry()->momentum();
    StiTrackPt = mom.perp();
    StiTrackQ  = newtrack->geometry()->charge();
    StiTrackPsi = newtrack->geometry()->psi();*/
}

void TrackEntry::setMcTrack(StMcTrack *newtrack)
{
  //cout << "Setting MC ID " << newtrack->geantId();
  mcTrackPt  = newtrack->pt();
  mcTrackId  = newtrack->geantId();
}
