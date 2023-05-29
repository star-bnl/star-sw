/***************************************************************************
 *
 * $Id: StFttCluster.cxx,v 1.0 2021/11/18 14:53:48 jdb Exp $
 *
 * Author: jdb, Nov 2021
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************/ 
#include "StEvent/StFttCluster.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StFttRawHit.h"

StFttCluster::StFttCluster() :
mId(-1),
mOrientation(kFttUnknownOrientation),
mNStrips(0),
mSumAdc(0.0),
mX(0.0),
mSigma(0.0),
mMaxADC(0),
mIndexMaxStrip(0),
mMaxStripCenter(0),
mMaxStripLeftEdge(0),
mMaxStripRightEdge(0),
mRawHits(0),
mNeighbors(0)
{

}


StFttCluster::~StFttCluster(){}

void StFttCluster::addPoint(StFttPoint* p) {
    mPoints.push_back(p);
}

void StFttCluster::addRawHit(StFttRawHit* p) {
    mRawHits.push_back(p);
}

void StFttCluster::addNeighbor(StFttCluster* neighbor) {
    int n=nNeighbors();
    for(int i=0; i<n; i++) if(mNeighbors[i]==neighbor) return; //already there, do nothing
        mNeighbors.push_back(neighbor);
}

std::ostream&
operator<<( std::ostream &os, const StFttCluster& rh )
{

    os << endl;
    os << "StFttCluster( "   << endl;
    os << "\tid          = " << rh.id()               << endl;
    os << "\tplane       = " << (int)rh.plane()       << endl;
    os << "\tquadrant    = " << (int)rh.quadrant()    << endl;
    os << "\torientation = " << (int)rh.orientation() << endl;
    os << "\tnStrips     = " << rh.nStrips()          << endl;
    os << "\tnPoints     = " << rh.nPoints()          << endl;
    os << "\tnRawHits    = " << rh.nRawHits()         << endl;
    os << "\tnNeighbors  = " << rh.nNeighbors()       << endl;
    os << "\tsumAdc      = " << rh.sumAdc()           << endl;
    os << "\tx           = " << rh.x()                << endl;
    os << "\tsigma       = " << rh.sigma()            << endl;
    os << ")"                << endl;
    return os;
}

void StFttCluster::print() {
    LOG_INFO << " Cluster with " << this->nRawHits() << " Hits" << endm;
    int i = 0;
    for (auto rawhit : mRawHits )
    {
        LOG_INFO << "Hit " << i << " with strip center = " << rawhit->stripCenter() << " with ADC = " << rawhit->adc() << endm;
        i++;
    }
}
