/***************************************************************************
 *
 * $Id: StTrack.cxx,v 2.47 2017/05/04 01:00:30 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.cxx,v $
 * Revision 2.47  2017/05/04 01:00:30  perev
 * Add Fts
 *
 * Revision 2.46  2016/11/28 21:00:24  ullrich
 * Added StExtGeometry features.
 *
 * Revision 2.45  2015/05/13 17:06:14  ullrich
 * Added hooks and interfaces to Sst detector (part of HFT).
 *
 * Revision 2.44  2013/07/23 11:21:49  jeromel
 * Undo past week changes
 *
 * Revision 2.42  2013/04/10 19:15:53  jeromel
 * Step back from StEvent changes - previous change recoverable [Thomas OK-ed]
 *
 * Revision 2.40  2013/01/15 23:21:06  fisyak
 * improve printouts
 *
 * Revision 2.39  2012/05/07 14:42:58  fisyak
 * Add handilings for Track to Fast Detectors Matching
 *
 * Revision 2.38  2011/10/17 00:13:49  fisyak
 * Add handles for IdTruth info
 *
 * Revision 2.37  2011/10/13 21:25:27  perev
 * setting IdTruth from the hits is added
 *
 * Revision 2.36  2011/04/26 21:41:29  fisyak
 * Make mKey Int_t instead of UShort_t (no. of tracks might be more that 64k)
 *
 * Revision 2.35  2011/03/31 19:29:01  fisyak
 * Add IdTruth information for tracks and vertices
 *
 * Revision 2.34  2010/08/31 20:00:09  fisyak
 * Clean up, add mSeedQuality
 *
 * Revision 2.33  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.32  2009/04/29 23:02:36  perev
 * check for big lenght added
 *
 * Revision 2.31  2007/10/11 21:51:40  ullrich
 * Added member to handle number of possible points fpr PXL and IST.
 *
 * Revision 2.30  2006/08/28 17:04:46  fisyak
 * Don't check StPhysicalHelixD quality for Beam Background tracks (flag() == 901)
 *
 * Revision 2.29  2005/07/06 19:00:52  fisyak
 * Add include of StThreeVectorD.hh
 *
 * Revision 2.28  2004/11/08 22:25:38  perev
 * Remove StTrack test for wrong length. TPT only
 *
 * Revision 2.27  2004/10/20 18:55:13  ullrich
 * Name of enum changed: StStarMaxR(Z) now StStarMaxTrackRangeR(Z).
 *
 * Revision 2.26  2004/10/17 03:35:10  perev
 * Error check improved
 *
 * Revision 2.25  2004/08/13 18:15:08  ullrich
 * Added +1 to the number of possible points when primary track.
 *
 * Revision 2.24  2004/08/10 14:20:21  calderon
 * Putting the streamers back in.  They should not be needed, but
 * apparently removing them causes more problems.  Yuri tested that
 * putting them back in allows reading files again.
 *
 * Revision 2.23  2004/08/05 22:24:51  ullrich
 * Changes to the handling of numberOfPoints() to allow ITTF more flexibility.
 *
 * Revision 2.22  2004/01/26 22:56:28  perev
 * Add Finite for float
 *
 * Revision 2.21  2003/12/04 03:53:14  perev
 * Set empty, instead of crazy outer geometry
 *
 * Revision 2.20  2003/10/31 16:00:04  ullrich
 * Added setKey() method.
 *
 * Revision 2.19  2003/10/30 20:07:32  perev
 * Check of quality added
 *
 * Revision 2.18  2003/04/25 23:48:18  calderon
 * fittingMethod member function was missing case for kITKalmanFitId.
 *
 * Revision 2.17  2002/03/14 17:42:31  ullrich
 * Added method to set mNumberOfPossiblePoints.
 *
 * Revision 2.16  2002/02/27 19:09:22  ullrich
 * Updated fittingMethod(): L3 added.
 *
 * Revision 2.15  2001/09/28 22:20:49  ullrich
 * Added helix geometry at last point.
 *
 * Revision 2.14  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.13  2001/04/05 04:00:57  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.12  2001/03/16 20:56:45  ullrich
 * Added non-const version of fitTraits().
 *
 * Revision 2.11  2000/04/20 13:49:07  ullrich
 * Removed redundant line in operator=().
 *
 * Revision 2.10  2000/01/20 14:42:40  ullrich
 * Fixed bug in numberOfPossiblePoints(). Sum was wrong.
 *
 * Revision 2.9  1999/12/01 15:58:08  ullrich
 * New decoding for dst_track::method. New enum added.
 *
 * Revision 2.8  1999/12/01 00:15:27  didenko
 * temporary solution to compile the library
 *
 * Revision 2.7  1999/11/29 17:32:42  ullrich
 * Added non-const method pidTraits().
 *
 * Revision 2.6  1999/11/15 18:48:20  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.5  1999/11/09 15:44:14  ullrich
 * Removed method unlink() and all calls to it.
 *
 * Revision 2.4  1999/11/05 15:27:04  ullrich
 * Added non-const versions of several methods
 *
 * Revision 2.3  1999/11/04 13:32:00  ullrich
 * Added non-const versions of some methods
 *
 * Revision 2.2  1999/11/01 12:45:02  ullrich
 * Modified unpacking of point counter
 *
 * Revision 2.1  1999/10/28 22:27:21  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:54  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <map>
#include "TClass.h"
#include "StMath.hh"
#include "StTrack.h"
#include "StParticleDefinition.hh"
#include "StVertex.h"
#include "StTrackGeometry.h"
#include "StTrackDetectorInfo.h"
#include "StTrackPidTraits.h"
#include "StTrackNode.h"
#include "StThreeVectorD.hh"
#include "StHit.h"
#include "StG2TrackVertexMap.h"
#include "StExtGeometry.h"
ClassImp(StTrack)

static const char rcsid[] = "$Id: StTrack.cxx,v 2.47 2017/05/04 01:00:30 perev Exp $";

StTrack::StTrack()
{
    memset(mBeg, 0, mEnd-mBeg+1);
}


StTrack::StTrack(const StTrack& track) {
    for (int bit = 14; bit < 23; bit++) if (track.TestBit(BIT(bit))) SetBit(BIT(bit));
    memcpy (mBeg, track.mBeg, mEnd-mBeg+1);
    mTopologyMap = track.mTopologyMap;
    mFitTraits = track.mFitTraits;
    if (track.mGeometry)
        mGeometry = track.mGeometry->copy();
    else
        mGeometry = 0;
    if (track.mOuterGeometry)
        mOuterGeometry = track.mOuterGeometry->copy();
    else
        mOuterGeometry = 0;
    mDetectorInfo = track.mDetectorInfo;       // not owner anyhow
    mPidTraitsVec = track.mPidTraitsVec;
    mNode = 0;                                 // do not assume any context here
}

StTrack&
StTrack::operator=(const StTrack& track) {
    if (this != &track) {
        for (int bit = 14; bit < 23; bit++) if (track.TestBit(BIT(bit))) SetBit(BIT(bit));
        memcpy (mBeg, track.mBeg, mEnd-mBeg+1);
        mTopologyMap = track.mTopologyMap;
        mFitTraits = track.mFitTraits;
        if (mGeometry) delete mGeometry;
        if (track.mGeometry)
            mGeometry = track.mGeometry->copy();
        else
            mGeometry = 0;
        if (mOuterGeometry) delete mOuterGeometry;
        if (track.mOuterGeometry)
            mOuterGeometry = track.mOuterGeometry->copy();
        else
            mOuterGeometry = 0;
        mDetectorInfo = track.mDetectorInfo;       // not owner anyhow
        mPidTraitsVec = track.mPidTraitsVec;
    }
    return *this;
}

StTrack::~StTrack()
{
    delete mGeometry;
    delete mOuterGeometry;
}

short
StTrack::flag() const { return mFlag; }


unsigned short
StTrack::encodedMethod() const { return mEncodedMethod; }

bool
StTrack::finderMethod(StTrackFinderMethod bit) const
{
    return mEncodedMethod & (1<<bit);
}

StTrackFittingMethod
StTrack::fittingMethod() const
{
    int method = mEncodedMethod & 0xf;
    switch(method) {
        case kHelix2StepId:
            return kHelix2StepId;
            break;
        case kHelix3DId:
            return kHelix3DId;
            break;
        case kKalmanFitId:
            return kKalmanFitId;
            break;
        case kLine2StepId:
            return kLine2StepId;
            break;
        case kLine3DId:
            return kLine3DId;
            break;
        case kL3FitId:
            return kL3FitId;
            break;
        case kITKalmanFitId:
            return kITKalmanFitId;
            break;
        default:
        case kUndefinedFitterId:
            return kUndefinedFitterId;
            break;
    }
}

float
StTrack::impactParameter() const { return mImpactParameter; }

float
StTrack::length() const { return mLength; }

unsigned short
StTrack::numberOfPossiblePoints() const
{
    unsigned short result;
    result = numberOfPossiblePoints(kTpcId) +
    numberOfPossiblePoints(kFtpcWestId) +
    numberOfPossiblePoints(kFtpcEastId) +
    numberOfPossiblePoints(kSvtId) +
    numberOfPossiblePoints(kSsdId) +
    numberOfPossiblePoints(kSstId) +
    numberOfPossiblePoints(kPxlId) +
    numberOfPossiblePoints(kIstId) +
    numberOfPossiblePoints(kFtsId);
    if (type() == primary || type() == estPrimary) result++;
    return result;
}

unsigned short
StTrack::numberOfPossiblePoints(StDetectorId det) const
{
    switch (det) {
        case kFtpcWestId:
            return mNumberOfPossiblePointsFtpcWest;
        case kFtpcEastId:
            return mNumberOfPossiblePointsFtpcEast;
        case kTpcId:
            return mNumberOfPossiblePointsTpc;
        case kSvtId:
            return mNumberOfPossiblePointsSvt;
        case kSsdId:
            return mNumberOfPossiblePointsSsd;
        case kSstId:
            return mNumberOfPossiblePointsSst;
        case kPxlId:
            return mNumberOfPossiblePointsPxl;
        case kIstId:
            return mNumberOfPossiblePointsIst;
        case kFtsId:
            return mNumberOfPossiblePointsFts;
        default:
            return 0;
    }
}

const StTrackTopologyMap&
StTrack::topologyMap() const { return mTopologyMap; }

const StTrackGeometry*
StTrack::geometry() const { return mGeometry; }

StTrackGeometry*
StTrack::geometry() { return mGeometry; }

const StTrackGeometry*
StTrack::outerGeometry() const { return mOuterGeometry; }

StTrackGeometry*
StTrack::outerGeometry() { return mOuterGeometry; }

StTrackFitTraits&
StTrack::fitTraits() { return mFitTraits; }

const StTrackFitTraits&
StTrack::fitTraits() const { return mFitTraits; }

StTrackDetectorInfo*
StTrack::detectorInfo() { return mDetectorInfo; }

const StTrackDetectorInfo*
StTrack::detectorInfo() const { return mDetectorInfo; }

const StSPtrVecTrackPidTraits&
StTrack::pidTraits() const { return mPidTraitsVec; }

StSPtrVecTrackPidTraits&
StTrack::pidTraits() { return mPidTraitsVec; }

StPtrVecTrackPidTraits
StTrack::pidTraits(StDetectorId det) const
{
    StPtrVecTrackPidTraits vec;
    for (unsigned int i=0; i<mPidTraitsVec.size(); i++)
        if (mPidTraitsVec[i]->detector() == det)
            vec.push_back(mPidTraitsVec[i]);
    return vec;
}

const StParticleDefinition*
StTrack::pidTraits(StPidAlgorithm& pid) const
{
    return pid(*this, mPidTraitsVec);
}

const StTrackNode*
StTrack::node() const { return mNode; }

StTrackNode*
StTrack::node() { return mNode; }

void
StTrack::setFlag(short val) { mFlag = val; }


void
StTrack::setEncodedMethod(unsigned short val) { mEncodedMethod = val; }

void
StTrack::setImpactParameter(float val) { mImpactParameter = val; }

void
StTrack::setLength(float val) { mLength = val; }

void
StTrack::setTopologyMap(const StTrackTopologyMap& val) { mTopologyMap = val; }

void
StTrack::setGeometry(StTrackGeometry* val)
{
    if (mGeometry) delete mGeometry;
    mGeometry = val;
}

void
StTrack::setOuterGeometry(StTrackGeometry* val)
{
    if (mOuterGeometry) delete mOuterGeometry;
    mOuterGeometry = val;
}

void
StTrack::setFitTraits(const StTrackFitTraits& val) { mFitTraits = val; }

void
StTrack::addPidTraits(StTrackPidTraits* val) { mPidTraitsVec.push_back(val); }

void
StTrack::setDetectorInfo(StTrackDetectorInfo* val) { mDetectorInfo = val; }


void
StTrack::setNumberOfPossiblePoints(unsigned char val, StDetectorId det)
{
    switch (det) {
        case kFtpcWestId:
            mNumberOfPossiblePointsFtpcWest = val;
            break;
        case kFtpcEastId:
            mNumberOfPossiblePointsFtpcEast = val;
            break;
        case kTpcId:
            mNumberOfPossiblePointsTpc = val;
            break;
        case kSvtId:
            mNumberOfPossiblePointsSvt = val;
            break;
        case kSsdId:
            mNumberOfPossiblePointsSsd = val;
            break;
        case kSstId:
            mNumberOfPossiblePointsSst = val;
            break;
        case kPxlId:
            mNumberOfPossiblePointsPxl = val;
            break;
        case kIstId:
            mNumberOfPossiblePointsIst = val;
            break;
        case kFtsId:
            mNumberOfPossiblePointsFts = val;
            break;
        default:
            break;
    }
}

void
StTrack::setNode(StTrackNode* val) { mNode = val; }

#include "StHelixModel.h"
int StTrack::bad() const
{
    static const double world = 1.e+5;
    int ierr;
    if (!StMath::Finite(mImpactParameter))	return   12;
    if (!StMath::Finite(mLength)         )    	return   13;
    if (mFlag  <0                        )	return   21;
    if (mFlag ==0                        )	return   31;
    if (::fabs(mImpactParameter)>world   )	return   22;
    if (::fabs(mLength)         >world   ) 	return   23;
    if (mLength <1./world	         )    	return   33;
    if (mLength >   world	         )    	return   34;
    if (!mGeometry                       )	return   24;
    ierr = mGeometry->bad();
    if (ierr                             )	return    4+100*ierr;
    if (!mOuterGeometry                  )	return   25;
    ierr = mOuterGeometry->bad();
    if (ierr                             )        return    5+100*ierr;
    
    const StTrackDetectorInfo *di = mDetectorInfo;
    if (!di                              )   	return   26;
    ierr = di->bad();
    if (ierr                             )   	return    6+100*ierr;
    if ((flag()/100)%10 == 9) return 0; // don't check Helix for Beam Background tracks
    StPhysicalHelixD hlx1 = mGeometry->helix();
    StThreeVectorD   ori2 = mOuterGeometry->origin();
    double len12 = hlx1.pathLength(ori2);
    double per = hlx1.period();
    while (len12<  0) len12+=per;
    while (len12>per) len12-=per;
    double tol = (len12)*0.2; if (tol<1) tol =1;
    //VP ignor for TPT    if (fabs(mLength-len12)>tol)                 return   43;
    
    if (fabs(hlx1.z(mLength))>kStarMaxTrackRangeZ) return   53;
    double qwe = pow(hlx1.x(mLength),2)+pow(hlx1.y(mLength),2);
    if (sqrt(qwe)>kStarMaxTrackRangeR)		 return   63;
    return 0;
}

void StTrack::Streamer(TBuffer &R__b)
{
    // Stream an object of class .
    
    if (R__b.IsReading()) {
        unsigned int R__s, R__c;
        Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
        if (R__v > 1) {
            Class()->ReadBuffer(R__b, this, R__v, R__s, R__c);
            return;
        }
        //====process old versions before automatic schema evolution
        StObject::Streamer(R__b);
        R__b >> mKey;
        R__b >> mFlag;
        
        //     R__b >> mEncodedMethod;
        UChar_t oldEncodedMethod;
        R__b >> oldEncodedMethod;
        mEncodedMethod=oldEncodedMethod;
        
        R__b >> mImpactParameter;
        R__b >> mLength;
        R__b >> mSeedQuality;
        mTopologyMap.Streamer(R__b);
        mFitTraits.Streamer(R__b);
        R__b >> mGeometry;
        
        //     R__b >> mDetectorInfo;
        R__b >> (StTrackDetectorInfo*&)mDetectorInfo;
        
        //     R__b >> mNode;
        R__b >> (StTrackNode*&)mNode;
        
        
        mPidTraitsVec.Streamer(R__b);
        
        R__b.CheckByteCount(R__s, R__c, Class());
        //====end of old versions
        
    } else {
        Class()->WriteBuffer(R__b,this);
    }
}


void StTrack::setIdTruth() // match with IdTruth
{
    
    const StTrackDetectorInfo* di = detectorInfo();
    if (!di) return;
    const StPtrVecHit& vh = di->hits();
    
    typedef std::map< int,float>  myMap_t;
    typedef std::pair<int,float>  myPair_t;
    typedef myMap_t::const_iterator myIter_t;
    myMap_t  idTruths;
    
    // 		Loop to store all the mc track keys and quality of every reco hit on the track.
    int nHits = vh.size(),id=0,qa=0;
    for (int hi=0;hi<nHits; hi++) {
        const StHit* rHit = vh[hi];
        id = rHit->idTruth(); if (!id) continue;
        qa = rHit->qaTruth(); if (!qa) qa = 1;
        idTruths[id]+=qa;
    }
    if (! idTruths.size()) return;		//no simu hits
    int tkBest=-1; float qaBest=0,qaSum=0;
    for (myIter_t it=idTruths.begin(); it!=idTruths.end();++it) {
        qaSum+=(*it).second;
        if ((*it).second<qaBest)	continue;
        tkBest=(*it).first; qaBest=(*it).second;
    }
    if (tkBest < 0 || tkBest> 0xffff) return;
    int avgQua= 100*qaBest/(qaSum+1e-10)+0.5;
    setIdTruth(tkBest,avgQua);
    int IdVx = StG2TrackVertexMap::instance()->IdVertex(tkBest);
    setIdParentVx(IdVx);
}
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StTrack& track) {
    os << Form("%4i ",track.key());
    if (track.type() == global)                       os << "global";
    else                                              os << "primary";
    os << Form(" %4i",track.flag());
    if (track.isPostXTrack())                         os << "C";
    else if (track.isPromptTrack())                   os << "P";
    else                                              os << " ";
    if (track.isCtbMatched() || track.isToFMatched()) os << "T";
    else                                              os << " ";
    if (track.isBemcMatched() || track.isEemcMatched()) {
        if (track.isBemcMatched())                      os << "b";
        else                                            os << "e";
        unsigned int fext = track.flagExtension() & 07;
        switch (fext) {
            case 0:    os << " "; break;
            case 1:    os << "M"; break;
            case 2:    os << "H"; break;
            case 3:    os << "E"; break;
            case 4:    os << "T"; break;
            case 5:    os << "W"; break;
            case 6:    os << "Z"; break;
            default:   os << "?"; break;
        }
    }  else                                           os << "  ";
    if (track.isMembraneCrossingTrack())              os << "X";
    else if (track.isWestTpcOnly())                   os << "W";
    else if (track.isEastTpcOnly())                   os << "E";
    else                                              os << " ";
    if (track.isShortTrack2EMC())                     os << "S";
    else                                              os << " ";
    if (track.isRejected())                           os << "R";
    else                                              os << " ";
    double length = track.length();
    if (length > 9999.) length = 9999.;
    os << Form(" NP %2d L %8.3f", track.numberOfPossiblePoints(),length);
    os << Form(" IdT: %4i Q:%3i", track.idTruth(), track.qaTruth());
    return os;
}
//________________________________________________________________________________
void StTrack::addExtGeometry(StExtGeometry *exg) 
{
   StExtGeometry ** kexg = &mExtGeometry;
   for (; *kexg; kexg = &((*kexg)->mNext)) {}
   *kexg = exg;
}
