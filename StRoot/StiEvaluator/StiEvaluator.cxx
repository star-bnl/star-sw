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
#include "Sti/StiHit.h"
#include "Sti/StiKalmanTrackNode.h"

//StiEvaluator includes
#include "StiEventAssociator.h"
#include "StiTrackPairInfo.h"
#include "StiEvaluator.h"

StiEvaluator* StiEvaluator::sinstance = 0;

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

void StiEvaluator::evaluateForEvent(const StiTrackContainer* trackStore)
{
    cout <<"\nStiEvaluator::evaluateForEvent()"<<endl;
    cout <<"\tNumber of StiTracks:\t"<<trackStore->size()<<endl;

    typedef StiEventAssociator::McToInfoPairMap McMap;
    typedef StiEventAssociator::InfoPair StiInfoPair;

    McMap& myMap = StiEventAssociator::instance()->mcToInfoPairMap();

    //cout <<"\tLoop on McToInfoPairMap"<<endl;
    for (McMap::iterator outer_it=myMap.begin(); outer_it!=myMap.end(); ++outer_it) {

	mEntry->clear();
	StMcTrack* mcTrack = (*outer_it).first;

	//cout <<"-- New McTrack: "<<mcTrack<<endl;

	StiTrackPairInfo& testInfo = (*outer_it).second.second;
	if ( testInfo.partnerMcTrack()==0 ) {// This McTrack was not found!
	    //Fill McTrack info:
	    //cout <<"Track not found, fill McTrack and Return"<<endl;
	    mEntry->setMcTrack(mcTrack, 0); //Dont incremnt the nFound counter
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
	    
	    for (McMap::iterator it=range.first; it!=range.second; ++it) {
		
		StiTrackPairInfo& info = (*it).second.second;
		
		if (info.commonTpcHits()>mostCommon) { //update, remember
		    mostCommon = info.commonTpcHits();
		    bestGlobalPair = (*it).second.first;
		    bestStiPair = &info;
		}
	    }
	    //End kludge
	    
	    if (bestStiPair!=0) { //Finally, we can fill!
		
		//Fill McTrack info:
		mEntry->setMcTrack(bestStiPair->partnerMcTrack(), 1); //incremnt the nFound counter by one

		//Check to see fi Global assoc worked!
		if (bestGlobalPair!=0) {
		    mEntry->setGlobalTrack(bestGlobalPair->partnerTrack());
		    mEntry->setGlobalAssoc(bestGlobalPair);
		}
		//We already know that we're safe here
		mEntry->setStiTrack(bestStiPair->partnerTrack());
		mEntry->setAssociation(*bestStiPair);
		mTree->Fill();
	    }
	}
    }
    cout <<"Done Evaluating for event"<<endl;
}

//old stuff/extras
/*
//Temp will break if you run!
StiTrackAssociator* associator = 0;
//= StiTrackAssociator::instance();
    
for (StiTrackContainer::stitrackvec::const_iterator it=trackStore->begin();
it!=trackStore->end(); ++it) {
StiTrack* temp = (*it);
	
//Now you've got the track, do what you want with it
StiKalmanTrack* track = dynamic_cast<StiKalmanTrack*>(temp);
if (!track) {
cout <<"StiEvaluator::evaluateForEvent(). ERROR:\t"
<<"Cast to Kalman track failed.  ABORT"<<endl;
return;
}

//Now we have an StiEvaluableTrack
//StTrackPairInfo* associatedPair = track->stTrackPairInfo();

StiTrackAssociator::AssocPair asPair = associator->associate(track);	
StTrackPairInfo* newInfo = asPair.first;
if (!newInfo) {
cout <<"StiEvaluator::evaluateForEvent().  ERROR:\tnewInfo==0"<<endl;
}
//temp test (MLM)	
//if (newInfo!=associatedPair) {
//cout <<"StiEvaluator::? newInfo!=associatedPair"<<endl;
//}
//End temp test (MLM)
	
else {
//Call some function to actually fill TTree object(s)
mEntry->clear();
mEntry->setMcTrack(newInfo->partnerMcTrack());
mEntry->setGlobalTrack(newInfo->partnerTrack());
mEntry->setGlobalAssoc(newInfo);
mEntry->setStiTrack(track);
mEntry->setAssociation(asPair.second); //New 11/20/01 (MLM)
	    	    
//Fill the hit entry
fillHitEntry(track);

mTree->Fill();
}
}
*/

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

ClassImp(StiHitEntry)

    StiHitEntry::StiHitEntry()
{
    reset();
}

StiHitEntry::~StiHitEntry()
{
   
}


void StiHitEntry::reset()
{
    hitPosition = hitRefAngle = hitLocalX = hitLocalY = hitLocalZ = 0.;
    hitGlobalX = hitGlobalY = hitGlobalZ;
    nodeAlpha = nodeLocalX = nodeLocalY = nodeLocalZ = nodeLocalEta = nodeLocalCurvature
	= nodeLocalTanLambda = nodeXCenter = nodeYCenter = nodeLocalChi2 = 0.;
    nodeHasHit = 0;
}

ClassImp(TrackEntry)

    TrackEntry::TrackEntry()
	: mArray(new TClonesArray("StiHitEntry",100))
{
}

void TrackEntry::addStiHitEntry(const StiHitEntry& hit)
{
    TClonesArray& cArr = *mArray;
    new(cArr[mHitCounter++]) StiHitEntry(hit);
}

void TrackEntry::clear()
{
    mArray->Clear();
    mHitCounter = 0;

    mcNTimesFound = 0;
    mcTrackId = mcTrackPsi = mcTrackRapidity = mcTrackE = 0.;
    mcTrackPx = mcTrackPy = mcTrackPz = mcTrackEta = 0.;
    mcTrackNTpcHits = mcTrackNSvtHits = mcTrackNFtpcHits = 0;
    
    globalTrackQ = 0;
    globalTrackM = globalTrackPsi = globalTrackChi2 = globalTrackNHit = 0.;
    globalTrackPx = globalTrackPy = globalTrackPz = globalTrackPt = globalTrackEta = 0.;
    globalTrackFitPoints = 0;
    globalTrackNAssocHits = globalTrackNAssocTpcHits = globalTrackNAssocSvtHits =
	globalTrackNAssocFtpcHits = 0;
    
    stiTrackM = stiTrackPsi = stiTrackChi2 = stiTrackNHit = 0.;
    stiTrackY = stiTrackTanL = stiTrackPx = stiTrackPy = stiTrackPz = stiTrackPt = stiTrackEta = 0.;
    stiTrackNHits = stiTrackNTpcHits = stiTrackNSvtHits = 0;
    stiTrackNAssocHits = stiTrackNAssocTpcHits = stiTrackNAssocSvtHits  = 0;

}

void TrackEntry::setStiTrack(const StiTrack *newtrack)
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
    stiTrackPt = mom.perp();
    stiTrackEta = mom.pseudoRapidity(); 
}

void TrackEntry::setAssociation(const StiTrackPairInfo& info)
{
    stiTrackNAssocHits = info.commonTpcHits()+info.commonSvtHits()+info.commonFtpcHits();
    stiTrackNAssocTpcHits = info.commonTpcHits();
    stiTrackNAssocSvtHits = info.commonFtpcHits();
}

void TrackEntry::setMcTrack(const StMcTrack *newtrack, unsigned int nTimesFound)
{
    //cout << "Setting MC ID " << newtrack->geantId();
    mcNTimesFound += nTimesFound;
    mcTrackId       = newtrack->geantId();
    mcTrackE        = newtrack->energy();
    mcTrackRapidity = newtrack->rapidity();
    const StThreeVectorF& momentum = newtrack->momentum();
    mcTrackPt = momentum.perp();
    mcTrackEta = momentum.pseudoRapidity();

    mcTrackNTpcHits = newtrack->tpcHits().size();
    mcTrackNSvtHits = newtrack->svtHits().size();
    mcTrackNFtpcHits = newtrack->ftpcHits().size();
}

void TrackEntry::setGlobalAssoc(const StTrackPairInfo* pr)
{
    globalTrackNAssocTpcHits = pr->commonTpcHits();
    globalTrackNAssocSvtHits = pr->commonSvtHits();
    globalTrackNAssocFtpcHits = 0;
    globalTrackNAssocHits = pr->commonTpcHits()+pr->commonSvtHits();
}

void TrackEntry::setGlobalTrack(const StTrack *newtrack)
{
    const StThreeVectorF& mom = newtrack->geometry()->momentum();
    globalTrackPx = mom.x();
    globalTrackPy = mom.y();
    globalTrackPz = mom.z();
    globalTrackPt = mom.perp();
    globalTrackEta  = mom.pseudoRapidity();
    globalTrackQ  = newtrack->geometry()->charge();
    globalTrackPsi = newtrack->geometry()->psi();
    globalTrackFitPoints = newtrack->fitTraits().numberOfFitPoints();
    globalTrackChi2 = newtrack->fitTraits().chi2();
  
}
