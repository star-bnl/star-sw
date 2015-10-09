/***************************************************************************
 *
 * $Id: StVertex.cxx,v 2.18 2015/10/09 17:46:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertex.cxx,v $
 * Revision 2.18  2015/10/09 17:46:15  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.17  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.15  2013/04/10 19:15:53  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.13  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.12  2011/10/17 15:35:49  fisyak
 * Comment's fix
 *
 * Revision 2.11  2011/10/17 00:13:49  fisyak
 * Add handles for IdTruth info
 *
 * Revision 2.10  2009/11/23 16:34:08  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.9  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.8  2001/05/30 17:45:55  perev
 * StEvent branching
 *
 * Revision 2.7  2001/04/05 04:00:59  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.6  2000/03/08 14:29:54  ullrich
 * New method probChiSquared() added.
 *
 * Revision 2.5  2000/02/10 16:32:19  ullrich
 * flag changed from unsigned to signed long
 *
 * Revision 2.4  2000/01/11 19:22:12  ullrich
 * Added non-const parent() method.
 *
 * Revision 2.3  1999/12/21 15:09:23  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/11/22 15:04:43  ullrich
 * Forgot to fill mPosition in constructor. Fixed now.
 *
 * Revision 2.1  1999/10/28 22:28:07  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:29  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <algorithm>
#include "Riostream.h"
#include "TClass.h"
#include "TFile.h"
#include "StVertex.h"
#include "StTrack.h"
#include "StG2TrackVertexMap.h"
#include "TString.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

ClassImp(StVertex)

static const char rcsid[] = "$Id: StVertex.cxx,v 2.18 2015/10/09 17:46:15 ullrich Exp $";
UInt_t StVertex::fgNoFitPointCutForGoodTrack = 15;
StVertex::StVertex()
{
    mType = kUndefinedVtxId;
    memset(mBeg, 0, mEnd-mBeg+1);
}

Int_t
StVertex::operator==(const StVertex& v) const
{
    return mType == v.mType &&
        mFlag == v.mFlag &&
        mPosition == v.mPosition &&
        mChiSquared == v.mChiSquared;
}

Int_t
StVertex::operator!=(const StVertex& v) const
{
    return !(v == *this);
}

StMatrixF
StVertex::covariantMatrix() const
{
    StMatrixF m(3,3);
    m(1,1) = mCovariantMatrix[0];
    m(1,2) = m(2,1) = mCovariantMatrix[1];
    m(2,2) = mCovariantMatrix[2];
    m(1,3) = m(3,1) = mCovariantMatrix[3];
    m(2,3) = m(3,2) = mCovariantMatrix[4];
    m(3,3) = mCovariantMatrix[5];
    return m;
}
    
StThreeVectorF
StVertex::positionError() const
{
    return StThreeVectorF(::sqrt(mCovariantMatrix[0]), ::sqrt(mCovariantMatrix[2]), ::sqrt(mCovariantMatrix[5]));
}

void StVertex::setParent(StTrack* val) { mParent = val; }
void StVertex::setCovariantMatrix(float val[6]) { copy(val, val+6, mCovariantMatrix); }

void StVertex::Streamer(TBuffer &R__b)
{
    // Stream an object of class .
    
    if (R__b.IsReading()) {
        UInt_t R__s, R__c;
        Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
        if (R__v > 1) {
            Class()->ReadBuffer(R__b, this, R__v, R__s, R__c);
            return;
        }
        //====process old versions before automatic schema evolution
        StMeasuredPoint::Streamer(R__b);
        R__b >> (Int_t&)mType;
        R__b >> mFlag;
        Int_t dumy;
        if (gFile && gFile->GetVersion() < 30000) {R__b >> dumy;}
        R__b.ReadFastArray(mCovariantMatrix,6);
        R__b >> mChiSquared;
        R__b >> mProbChiSquared;
        //     R__b >> mParent;
        R__b >> (StTrack*&)mParent;
        
        R__b.CheckByteCount(R__s, R__c, Class());
        //====end of old versions
        
    }
    else {
        Class()->WriteBuffer(R__b,this);
    }
} 

void StVertex::setIdTruth() { // match with IdTruth
    typedef std::map< Int_t,Float_t>  myMap_t;
    typedef std::pair<Int_t,Float_t>  myPair_t;
    typedef myMap_t::const_iterator myIter_t;
    myMap_t  idTruths;
    // Loop to store all the mc vertex keys and quality of every reco track on the vertex.
    UInt_t Ntracks = numberOfDaughters();
    Int_t IdVx = 0;
    Int_t qa = 0;
    for (UInt_t l = 0; l < Ntracks; l++) {
        const StTrack *pTrack = daughter(l);
        if (! pTrack) continue;
        Int_t IdTk = pTrack->idTruth();
        if (IdTk <= 0) continue;
        IdVx = pTrack->idParentVx();
        if (IdVx <= 0) continue;
        qa = pTrack->qaTruth(); if (!qa) qa = 1;
        idTruths[IdVx] += qa;
    }
    if (! idTruths.size()) return;		//no simu info
    Int_t vxBest = 0;
    Float_t qaBest = 0, qaSum = 0;
    for (myIter_t it=idTruths.begin(); it!=idTruths.end(); ++it) {
        qaSum += (*it).second;
        if ((*it).second < qaBest) continue;
        vxBest = (*it).first; qaBest = (*it).second;
    }
    if (vxBest <= 0 || vxBest > 0xffff) return;
    Int_t avgQua = 100*qaBest/(qaSum+1e-10)+0.5;
    setIdTruth(vxBest,avgQua);
    Int_t IdParentTk = StG2TrackVertexMap::instance()->IdParentTrack(vxBest);
    setIdParent(IdParentTk);
}

void StVertex::NotImplemented(const char *method) const {
    cout << "StVertex::" << method << " is no implemented" <<  endl;
}


