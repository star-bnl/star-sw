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
	mEntry->setStiTrack(track);
	mTree->Fill();

    }
}

//Temp, to be moved to own file


ClassImp(TrackEntry)

TrackEntry::TrackEntry() 
{
}

void TrackEntry::setStiTrack(StiTrack *newtrack)
{
    //I'd shy away from the spaces, they tend to look different in everyone's editor
    
  stiTrackQ          = newtrack->getCharge();
  stiTrackPsi        = newtrack->getPhi();
  stiTrackM          = newtrack->getMass();
  stiTrackChi2       = newtrack->getChi2();
  stiTrackNHit       = newtrack->getFitPointCount();
  stiTrackY          = newtrack->getRapidity();
  stiTrackTanL       = newtrack->getTanL();

  StThreeVector<double> mom = newtrack->getMomentumAtOrigin();
  stiTrackPx = mom.x();
  stiTrackPy = mom.y();
  stiTrackPz = mom.z();
 
}

void TrackEntry::setMcTrack(StMcTrack *newtrack)
{
  //cout << "Setting MC ID " << newtrack->geantId();
  mcTrackId       = newtrack->geantId();
  mcTrackE        = newtrack->energy();
  mcTrackRapidity = newtrack->rapidity();
}

void TrackEntry::clear()
{
    mcTrackId = mcTrackPsi = 0.;
    globalTrackQ = 0;
    globalTrackM = globalTrackPsi = globalTrackChi2 = globalTrackNHit = 0.;
    stiTrackM = stiTrackPsi = stiTrackChi2 = stiTrackNHit = 0.;
}

void TrackEntry::setGlobalTrack(StTrack *newtrack)
{
  const StThreeVectorF& mom = newtrack->geometry()->momentum();
  globalTrackPx = mom.x();
  globalTrackPy = mom.y();
  globalTrackPz = mom.z();
  globalTrackEta  = mom.pseudoRapidity();
  globalTrackQ  = newtrack->geometry()->charge();
  globalTrackPsi = newtrack->geometry()->psi();
  //we can go from c casts (double) (x) to c++cast static_cast<double>(x).  Easier to read, safer
  globalTrackFitPoints = static_cast<double>(newtrack->fitTraits().numberOfFitPoints());
  globalTrackChi2 = newtrack->fitTraits().chi2();
  
}
