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

StiEvaluator::StiEvaluator() : mFile(0), mNtuple(0), mTree(0), mEntry(0)
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
    mNtuple = new TNtuple("ntuple","This is the ntuple","McID:McPt");

    //cout <<"Make TTree, here goes nothin'"<<endl;

    //cout <<"\tMake Entry"<<endl;
    mEntry = new TrackEntry();

    //cout <<"\tDeclare Tree"<<endl;
    mTree = new TTree("TestTree","The Test Tree");

    Int_t buffsize = 64000;
    Int_t splitlevel = 1;

    //cout <<"\tMake Branch, maybe seg-fault?"<<endl;
    mTree->Branch("TestBranch","TrackEntry",&mEntry, buffsize, splitlevel);

    //cout <<"\tSo far so good"<<endl;
    cout <<"\tdone"<<endl;
    
}

void StiEvaluator::evaluateForEvent(const StiTrackContainer* trackStore)
{
    cout <<"\nStiEvaluator::evaluateForEvent()"<<endl;
    cout <<"\tNumber of StiTracks:\t"<<trackStore->size()<<endl;
    //stitrackvec is a typedef that is within the public namespace of StiTrackContainer, so it is not a global typedef
    // it looks like this
    //class StiTrackContainer
    //{
    //public:
    //typedef vector<StiTrack*> stitrackvec; ...

    //Temporary test of filling
    //fillTuple();
    //mTree->Fill();
    
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
	mEntry->setTptTrack(associatedPair->partnerTrack());
	//mEntry->setStiTrack(temp);
	fillTree(temp, associatedPair);
	mTree->Fill();

    }
}

void StiEvaluator::fillTree(StiTrack *track, StTrackPairInfo *associatedPair)
{


    //Try the tree!
    mEntry->clear();
    
    mEntry->setA(1.2);
    mEntry->setB(2.3);

    
    //ArrayEntry myArrayEntry;

    mEntry->setMcTrack(associatedPair->partnerMcTrack());
    mEntry->setTptTrack(associatedPair->partnerTrack());

    
    //myArrayEntry.setStiTrack(track);


    //mNtuple->Fill(getMcTrackID(),getMcTrackPt());

    //mEntry->addArrayEntry(myArrayEntry);
   
}

//Temp, to be moved to own file



double TrackEntry::getMcTrackID(){
  return(McTrackID);
}

double TrackEntry::getMcTrackPt(){
  return(McTrackPt);
}

void TrackEntry::setTptTrack(StTrack *newtrack)
{
  const StThreeVectorF& mom = newtrack->geometry()->momentum();
  TptTrackPt = mom.perp();
  TptTrackQ  = newtrack->geometry()->charge();
  TptTrackPsi = newtrack->geometry()->psi();
}

void TrackEntry::setStiTrack(StiTrack *newtrack)
{
  /*const StThreeVectorD& mom = newtrack->geometry()->momentum();
  StiTrackPt = mom.perp();
  StiTrackQ  = newtrack->geometry()->charge();
  StiTrackPsi = newtrack->geometry()->psi();*/
}

void TrackEntry::setMcTrack(StMcTrack *newtrack)
{
  //cout << "Setting MC ID " << newtrack->geantId();
  McTrackPt  = newtrack->pt();
  McTrackID  = newtrack->geantId();
}

ClassImp(TrackEntry)
    
void TrackEntry::setA(double val)
{
    ma=val;
}

TrackEntry::TrackEntry() : mArray(new TClonesArray("ArrayEntry",50)), mCounter(0)
{
}

void TrackEntry::clear()
{
    mArray->Clear();
    mCounter=0;
}

/*void TrackEntry::addArrayEntry(const ArrayEntry& val)
{
    TClonesArray& cArr = *mArray;
    new(cArr[mCounter++]) ArrayEntry(val);
    }*/

void TrackEntry::setB(double val)
{
    mb=val;
}

double TrackEntry::a() const
{
    return ma;
}

double TrackEntry::b() const
{
    return mb;
}



