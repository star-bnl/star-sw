/***************************************************************************
 *
 * $Id: StEmcCluster.cxx,v 2.3 2000/07/31 22:12:22 akio Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcCluster.cxx,v $
 * Revision 2.3  2000/07/31 22:12:22  akio
 * eliminate print() for L3(?)
 *
 * Revision 2.2  2000/03/23 22:24:06  akio
 * Initial version of Emc Point, and Inclusion of track pointers
 *
 * Revision 2.1  2000/02/23 17:34:01  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcCluster.h"

ClassImp(StEmcCluster)
    
static const char rcsid[] = "$Id: StEmcCluster.cxx,v 2.3 2000/07/31 22:12:22 akio Exp $";

StEmcCluster::StEmcCluster() {/* noop*/};

StEmcCluster::~StEmcCluster() {/* noop */};

Float_t
StEmcCluster::eta() const {return mEta;} 

Float_t
StEmcCluster::phi() const {return mPhi;}

Float_t
StEmcCluster::sigmaEta() const {return mSigmaEta;}

Float_t
StEmcCluster::sigmaPhi() const {return mSigmaPhi;}

Float_t
StEmcCluster::energy() const {return mEnergy;}

Int_t
StEmcCluster::nHits() const {return mHits.size();}

Int_t
StEmcCluster::nNeighbors() const {return mNeighbors.size();}

Int_t
StEmcCluster::nTracks() const {return mTracks.size();}

StPtrVecEmcRawHit&
StEmcCluster::hit() {return mHits;}

const StPtrVecEmcRawHit&
StEmcCluster::hit() const {return mHits;}

StPtrVecEmcCluster&
StEmcCluster::neighbor() {return mNeighbors;}

const StPtrVecEmcCluster&
StEmcCluster::neighbor() const {return mNeighbors;}

StPtrVecTrack&
StEmcCluster::track() {return mTracks;}

const StPtrVecTrack&
StEmcCluster::track() const {return mTracks;}

void
StEmcCluster::setEta(Float_t ver)      {mEta=ver;}

void
StEmcCluster::setPhi(Float_t ver)      {mPhi=ver;}

void
StEmcCluster::setSigmaEta(Float_t ver) {mSigmaEta=ver;}

void
StEmcCluster::setSigmaPhi(Float_t ver) {mSigmaPhi=ver;}

void
StEmcCluster::setEnergy(Float_t ver)   {mEnergy=ver;}

void
StEmcCluster::addHit(StEmcRawHit* hit) {mHits.push_back(hit);}

void
StEmcCluster::addNeighbor(StEmcCluster* cluster) {mNeighbors.push_back(cluster);}

void
StEmcCluster::addTrack(StTrack* track) {mTracks.push_back(track);}

StObject*
StEmcCluster::clone() {return new StEmcCluster(*this);}

ostream&
operator<<(ostream &os, const StEmcCluster& cl)
{ 
  return (os << " Energy " << cl.energy() << endl
	  << " Eta "    << cl.eta() << "+/-" << cl.sigmaEta() << endl 
	  << " Phi "    << cl.phi() << "+/-" << cl.sigmaPhi() << endl
	  << " # of hits " << cl.nHits() << ",# of neighbor " << cl.nNeighbors() << endl);
}

