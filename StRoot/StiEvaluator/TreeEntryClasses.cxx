//TreeEntryClasses.cxx
//M.L. Miller (Yale Software)
//2/02

//ROOT
#include "TFile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TClonesArray.h"

//STD
#include <stdexcept>
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
#include "TreeEntryClasses.h"

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
    hitTimesUsed=0;
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

    mcNTimesFound = bestMatch = 0;
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
    stiTrackFlag = stiTrackNSeedHits = 0;

}

void TrackEntry::setStiTrack(const StiTrack *newtrack)
{
    stiTrackFlag=0;
    StThreeVector<double> mom;
    
    try {
	stiTrackQ          = newtrack->getCharge();
	stiTrackPsi        = newtrack->getPhi();
	stiTrackM          = newtrack->getMass();
	stiTrackChi2       = newtrack->getChi2();
	stiTrackNHit       = newtrack->getPointCount();
	stiTrackNSeedHits = newtrack->getSeedHitCount();
	stiTrackY          = newtrack->getRapidity();
	stiTrackTanL       = newtrack->getTanL();
	mom = newtrack->getMomentumAtOrigin();
	stiTrackPx = mom.x();
	stiTrackPy = mom.y();
	stiTrackPz = mom.z();
	stiTrackPt = mom.perp();
	stiTrackEta = mom.pseudoRapidity();
    }
    
    catch (runtime_error & rte)	{
	//cout << "RunTime Error Exception: " << rte.what()<<endl;
    }
    catch (exception & e) {
	//cout << "Exception: " << e.what()<<endl;
    }
    
    //We made it all the way through, mark this track as legit
    stiTrackFlag = 100 + newtrack->getFlag();
}

void TrackEntry::setAssociation(const StiTrackPairInfo& info)
{
    stiTrackNAssocHits = info.commonTpcHits()+info.commonSvtHits()+info.commonFtpcHits();
    stiTrackNAssocTpcHits = info.commonTpcHits();
    stiTrackNAssocSvtHits = info.commonFtpcHits();
}

void TrackEntry::setMcTrack(const StMcTrack *newtrack, unsigned int nTimesFound, bool best)
{
    //cout << "Setting MC ID " << newtrack->geantId();
    mcNTimesFound += nTimesFound;
    bestMatch = (best==true) ? 1 : 0;
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
