/***************************************************************************
 *
 * $Id: StTrackPidTraits.cxx,v 2.2 1999/11/15 18:48:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTrackPidTraits.cxx,v $
 * Revision 2.2  1999/11/15 18:48:25  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.3  1999/11/29 16:53:24  ullrich
 * ADopted new encoding scheme for method().
 *
 * Revision 2.2  1999/11/15 18:48:25  ullrich
 * Adapted new enums for dedx and track reco methods.
 *
 * Revision 2.1  1999/10/28 22:27:49  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:43:05  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include "tables/St_dst_dedx_Table.h"
#include "StTrackPidTraits.h"

ClassImp(StTrackPidTraits)

static const char rcsid[] = "$Id: StTrackPidTraits.cxx,v 2.2 1999/11/15 18:48:25 ullrich Exp $";

StTrackPidTraits::StTrackPidTraits() :
    mDetectorId(0), mMethod(0) { /* noop */ }

StTrackPidTraits::StTrackPidTraits(StDetectorId det, Short_t meth) :
    mDetectorId(det), mMethod(meth) { /* noop */ }

StTrackPidTraits::StTrackPidTraits(const dst_dedx_st& t) :
    mDetectorId(t.det_id), mMethod(t.method) { /* noop */ }

StTrackPidTraits::~StTrackPidTraits() { /* noop */ }

Short_t
StTrackPidTraits::encodedMethod() const { return mMethod; }

    switch (mMethod%100) {
StTrackPidTraits::method() const
{
    switch (mMethod) {
    case kTruncatedMeanId:
	return kTruncatedMeanId;
	break;
    case kEnsembleTruncatedMeanId:
	return kEnsembleTruncatedMeanId;
	break;
    case kLikelihoodFitId:
	return kLikelihoodFitId;
	break;
    case kWeightedTruncatedMeanId:
	return kWeightedTruncatedMeanId;
	break;
    case kOtherMethodId:
	return kOtherMethodId;
	break;
    default:
	return kUndefinedMethodId;
	break;
    }
}

Short_t
StTrackPidTraits::detector() const { return mDetectorId; }
