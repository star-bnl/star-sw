/***************************************************************************
 *
 * $Id: StTrack.cxx,v 2.19 2003/10/30 20:07:32 perev Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.cxx,v $
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
#include "TClass.h"
#include "StTrack.h"
#include "tables/St_dst_track_Table.h"
#include "StParticleDefinition.hh"
#include "StVertex.h"
#include "StTrackGeometry.h"
#include "StTrackDetectorInfo.h"
#include "StTrackPidTraits.h"
#include "StTrackNode.h"

ClassImp(StTrack)

static const char rcsid[] = "$Id: StTrack.cxx,v 2.19 2003/10/30 20:07:32 perev Exp $";

StTrack::StTrack()
{
    mFlag = 0;
    mKey = 0;
    mEncodedMethod = 0;
    mImpactParameter = 0;
    mLength = 0;
    mNumberOfPossiblePoints = 0;
    mGeometry = 0;
    mOuterGeometry = 0;
    mDetectorInfo = 0;
    mNode = 0;
}

StTrack::StTrack(const dst_track_st& track) :
    mTopologyMap(track.map), mFitTraits(track)
{
    mKey = track.id;
    mFlag = track.iflag;
    mEncodedMethod = track.method;
    mImpactParameter = track.impact;
    mLength = track.length;
    mNumberOfPossiblePoints = track.n_max_point;
    mGeometry = 0;                                // has to come from outside
    mOuterGeometry = 0;                           // has to come from outside
    mDetectorInfo = 0;                            // has to come from outside
    mNode = 0;                                    // has to come from outside
}

StTrack::StTrack(const StTrack& track)
{
    mKey = track.mKey;
    mFlag = track.mFlag;
    mEncodedMethod = track.mEncodedMethod;
    mImpactParameter = track.mImpactParameter;
    mLength = track.mLength;
    mNumberOfPossiblePoints = track.mNumberOfPossiblePoints;
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
StTrack::operator=(const StTrack& track)
{
    if (this != &track) {
        mFlag = track.mFlag;
        mKey = track.mKey;
        mEncodedMethod = track.mEncodedMethod;
        mImpactParameter = track.mImpactParameter;
        mLength = track.mLength;
        mNumberOfPossiblePoints = track.mNumberOfPossiblePoints;
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
        mNode = 0;                                 // do not assume any context here
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
StTrack::key() const { return mKey; }

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
    return (numberOfPossiblePoints(kTpcId) +
            numberOfPossiblePoints(kSvtId) +
            numberOfPossiblePoints(kSsdId));
}

unsigned short
StTrack::numberOfPossiblePoints(StDetectorId det) const
{
    // 1*tpc + 1000*svt + 10000*ssd (Helen/Spiros Oct 29, 1999)
    switch (det) {
    case kFtpcWestId:
    case kFtpcEastId:
    case kTpcId:
        return mNumberOfPossiblePoints%1000;
        break;
    case kSvtId:
        return (mNumberOfPossiblePoints%10000)/1000;
        break;
    case kSsdId:
        return mNumberOfPossiblePoints/10000;
        break;
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
StTrack::setNumberOfPossiblePoints(unsigned short val) {mNumberOfPossiblePoints = val;}

void
StTrack::setNode(StTrackNode* val) { mNode = val; }

int StTrack::bad() const
{
static const double world = 1.e+5;
 if (mFlag <=0                     )            return 01;
 if (!::finite(mImpactParameter)   ) 		return 10;
 if (::fabs(mImpactParameter)>world) 		return 11;
 if (!::finite(mLength)            )    	return 20;
 if (::fabs(mLength)         >world) 		return 21;
 if (mLength <1./world	           )    	return 22;
 if (mGeometry      && mGeometry->bad()     )	return 30;
 if (mOuterGeometry && mOuterGeometry->bad())   return 40;
 const StTrackDetectorInfo *di = mDetectorInfo;
 if (di             && di->bad()            )   return 50;
return 0;

}
void StTrack::Streamer(TBuffer &R__b)
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
       StObject::Streamer(R__b);
       R__b >> mKey;
       R__b >> mFlag;

//     R__b >> mEncodedMethod;
       UChar_t oldEncodedMethod;
       R__b >> oldEncodedMethod;
       mEncodedMethod=oldEncodedMethod;

       R__b >> mImpactParameter;
       R__b >> mLength;
       R__b >> mNumberOfPossiblePoints;
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


