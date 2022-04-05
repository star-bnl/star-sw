/********************************************************************
 *
 * $Id: StPhmdCluster.cxx,v 2.1 2002/12/20 22:33:00 ullrich Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: StPhmdCluster is base class for PMD cluster. 
 *
 ********************************************************************
 *
 * $Log: StPhmdCluster.cxx,v $
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#include "StPhmdCluster.h"


ClassImp(StPhmdCluster)

StPhmdCluster::StPhmdCluster()
{
    mModule = 0;          
    mNumberOfCells = 0;   
    mEta = 0;             
    mPhi = 0;             
    mEnergy = 0;          
    mSigma = 0;           
    mPID = 0;             
    mEnergyPID = 0;       
    mMcPID = 0;
}

StPhmdCluster::~StPhmdCluster(){ /* noop */ }

StPtrVecPhmdHit&
StPhmdCluster::hit() {return mHits;}

const StPtrVecPhmdHit&
StPhmdCluster::hit() const {return mHits;}

void
StPhmdCluster::addHit(StPhmdHit* hit) {mHits.push_back(hit);}

ostream& operator<<(ostream &os, const StPhmdCluster& c)
{
    os << "module="     << c.module();
    os << "\tcells="    << c.numberOfCells();
    os << "\tenergy="   << c.energy();
    os << "\teta="      << c.eta();
    os << "\tphi="      << c.phi();
    return os;
}
