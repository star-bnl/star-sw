/***************************************************************************
 *
 * $Id: StEmcCluster.cxx,v 2.1 2000/02/23 17:34:01 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcCluster.cxx,v $
 * Revision 2.1  2000/02/23 17:34:01  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcCluster.h"

ClassImp(StEmcCluster)
    
static const char rcsid[] = "$Id: StEmcCluster.cxx,v 2.1 2000/02/23 17:34:01 ullrich Exp $";

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

StPtrVecEmcRawHit&
StEmcCluster::hit() {return mHits;}

const StPtrVecEmcRawHit&
StEmcCluster::hit() const {return mHits;}

StPtrVecEmcCluster&
StEmcCluster::neighbor() {return mNeighbors;}

const StPtrVecEmcCluster&
StEmcCluster::neighbor() const {return mNeighbors;}

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

StObject*
StEmcCluster::clone() {return new StEmcCluster(*this);}

void
StEmcCluster::print(ostream& os) const
{
    os << " Energy " << mEnergy << endl;
    os << " Eta "    << mEta << "+/-" << mSigmaEta << endl;
    os << " Phi "    << mPhi << "+/-" << mSigmaPhi << endl;
    os << " # of hits " << nHits() << ",# of neighbor " << nNeighbors() << endl;
}

ostream&
operator<<(ostream &os, const StEmcCluster& cl)
{ 
    cl.print(os);
    return os;
}
