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

//Association
#include "StAssociationMaker/StTrackPairInfo.hh"

//Sti includes
#include "Sti/StiTrackContainer.h"
#include "Sti/StiEvaluableTrack.h"

//StiMaker includes
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
    mNtuple = new TNtuple("ntuple","This is the ntuple","a:b:c");

    //cout <<"Make TTree, here goes nothin'"<<endl;

    //cout <<"\tMake Entry"<<endl;
    mEntry = new TreeEntry();

    //cout <<"\tDeclare Tree"<<endl;
    mTree = new TTree("TestTree","The Test Tree");

    Int_t buffsize = 64000;
    Int_t splitlevel = 1;

    //cout <<"\tMake Branch, maybe seg-fault?"<<endl;
    mTree->Branch("TestBranch","TreeEntry",&mEntry, buffsize, splitlevel);

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
    fillTuple();
    mTree->Fill();
    
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
	//Call some function to actuall fill TTree object(s)
    }
}

void StiEvaluator::fillTuple()
{
    for (double x=1.; x<10.; ++x) {
	mNtuple->Fill(x, 2.*x, 3.*x);
    }

    //Try the tree!
    mEntry->clear();
    
    mEntry->setA(1.2);
    mEntry->setB(2.3);

    for (double val=100.; val<1000.; ++val) {
	ArrayEntry myArrayEntry;
	myArrayEntry.setVal(val);
	mEntry->addArrayEntry(myArrayEntry);
    }

}

//Temp, to be moved to own file

ClassImp(ArrayEntry)

void ArrayEntry::setVal(double val)
{
    mval=val;
}

ClassImp(TreeEntry)
    
    void TreeEntry::setA(double val)
{
    ma=val;
}

TreeEntry::TreeEntry() : mArray(new TClonesArray("ArrayEntry",50)), mCounter(0)
{
}

void TreeEntry::clear()
{
    mArray->Clear();
    mCounter=0;
}

void TreeEntry::addArrayEntry(const ArrayEntry& val)
{
    TClonesArray& cArr = *mArray;
    new(cArr[mCounter++]) ArrayEntry(val);
}

void TreeEntry::setB(double val)
{
    mb=val;
}

double TreeEntry::a() const
{
    return ma;
}

double TreeEntry::b() const
{
    return mb;
}
