/***************************************************************************
 *
 * $Id: StTrack.cxx,v 2.5 1999/11/09 15:44:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrack.cxx,v $
 * Revision 2.5  1999/11/09 15:44:14  ullrich
 * Removed method unlink() and all calls to it.
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
#include "StTrack.h"
#include "tables/St_dst_track_Table.h"
static const char rcsid[] = "$Id: StTrack.cxx,v 2.5 1999/11/09 15:44:14 ullrich Exp $";
#include "StVertex.h"
#include "StTrackGeometry.h"
    mReconstructionMethod = 0;
#include "StTrackPidTraits.h"
#include "StTrackNode.h"

ClassImp(StTrack)

static const char rcsid[] = "$Id: StTrack.cxx,v 2.5 1999/11/09 15:44:14 ullrich Exp $";

StTrack::StTrack()
{
    mFlag = 0;
    mKey = 0;
    mEncodedMethod = 0;
    mImpactParameter = 0;
    mReconstructionMethod = track.method;
    mNumberOfPossiblePoints = 0;
    mGeometry = 0;
    mDetectorInfo = 0;
    mNode = 0;
}

StTrack::StTrack(const dst_track_st& track) :
    mTopologyMap(track.map), mFitTraits(track)
{
    mKey = track.id;
    mFlag = track.iflag;
    mEncodedMethod = track.method;
    mReconstructionMethod = track.mReconstructionMethod;
    mLength = track.length;
    mNumberOfPossiblePoints = track.n_max_point;
    mGeometry = 0;                                // has to come from outside
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
    mDetectorInfo = track.mDetectorInfo;       // not owner anyhow
        mReconstructionMethod = track.mReconstructionMethod;
    mNode = 0;                                 // do not assume any context here
}

StTrack&
StTrack::operator=(const StTrack& track)
{
    if (this != &track) {
        if (mGeometry)
            delete mGeometry;
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
        mDetectorInfo = track.mDetectorInfo;       // not owner anyhow
	mPidTraitsVec = track.mPidTraitsVec;
        mNode = 0;                                 // do not assume any context here
    }
    return *this;
}
UChar_t
StTrack::reconstructionMethod() const { return mReconstructionMethod; }
    case kLine2StepId:
	return kLine2StepId;
	break;
    case kLine3DId:
	return kLine3DId;
	break;
    default:
    case kUndefinedFitterId:
	return kUndefinedFitterId;
	break;
    }
}

Float_t
StTrack::impactParameter() const { return mImpactParameter; }

Float_t
StTrack::length() const { return mLength; }

    // 1*tpc + 1000*svt + 10000*ssd (Helen/Spiros Oct 29, 1999)
    return (mNumberOfPossiblePoints%1000) +
	((mNumberOfPossiblePoints%10000)/1000) +
	(mNumberOfPossiblePoints/10000);
    return (numberOfPossiblePoints(kTpcId) +
	    numberOfPossiblePoints(kSvtId) +
	    numberOfPossiblePoints(kSsdId));
}

UShort_t
StTrack::numberOfPossiblePoints(StDetectorId det) const
    switch (det) {
    case kFtpcWestId:
    case kFtpcEastId:
    case kTpcId:
	return mNumberOfPossiblePoints%1000;
	break;
    case kSvtId:
	return (mNumberOfPossiblePoints%10000)/1000;
	break;
    case kFtpcWestId:
    case kFtpcEastId:
	return mNumberOfPossiblePoints;
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

const StTrackFitTraits&
StTrack::fitTraits() const { return mFitTraits; }

const StPtrVecTrackPidTraits
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
StTrack::setReconstructionMethod(UChar_t val) { mReconstructionMethod = val; }

const StTrackNode*
StTrack::node() const { return mNode; }

StTrackNode*
StTrack::node() { return mNode; }

void
StTrack::setFlag(Short_t val) { mFlag = val; }

void
StTrack::setEncodedMethod(UShort_t val) { mEncodedMethod = val; }

void
StTrack::setImpactParameter(Float_t val) { mImpactParameter = val; }

void
StTrack::setLength(Float_t val) { mLength = val; }

void
StTrack::setTopologyMap(const StTrackTopologyMap& val) { mTopologyMap = val; }

void
StTrack::setGeometry(StTrackGeometry* val)
{
    if (mGeometry) delete mGeometry;
    mGeometry = val;
}

void
StTrack::setFitTraits(const StTrackFitTraits& val) { mFitTraits = val; }

void
StTrack::addPidTraits(StTrackPidTraits* val) { mPidTraitsVec.push_back(val); }

void
StTrack::setDetectorInfo(StTrackDetectorInfo* val) { mDetectorInfo = val; }

void
StTrack::setNode(StTrackNode* val) { mNode = val; }
