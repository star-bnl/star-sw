/***************************************************************************
 *
 * $Id: StDedxPidTraits.cxx,v 2.9 2001/04/05 04:00:47 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPidTraits.cxx,v $
 * Revision 2.9  2001/04/05 04:00:47  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.8  2001/03/24 03:34:40  perev
 * clone() -> clone() const
 *
 * Revision 2.7  2000/12/18 17:25:13  fisyak
 * Add track length used in dE/dx calculations
 *
 * Revision 2.6  2000/01/05 16:04:11  ullrich
 * Changed method name sigma() to errorOnMean().
 *
 * Revision 2.5  1999/11/29 17:07:24  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
 *
 * Revision 2.4  1999/11/23 15:56:23  ullrich
 * Added clone() const method. Was pure virtual.
 *
 * Revision 2.3  1999/11/16 14:11:38  ullrich
 * Changed variance to sigma.
 *
 * Revision 2.2  1999/10/28 22:25:01  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:31  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "tables/St_dst_dedx_Table.h"
#include "StDedxPidTraits.h"

ClassImp(StDedxPidTraits)

static const char rcsid[] = "$Id: StDedxPidTraits.cxx,v 2.9 2001/04/05 04:00:47 ullrich Exp $";

StDedxPidTraits::StDedxPidTraits() :
    mNumberOfPoints(0), mDedx(0), mSigma(0), mMethod(0) { /* noop */ }

StDedxPidTraits::StDedxPidTraits(StDetectorId det, short meth,
                                 unsigned short n, float dedx, float sig) :
    StTrackPidTraits(det),
    mNumberOfPoints(n), mDedx(dedx), mSigma(sig), mMethod(meth) { /* noop */ }

StDedxPidTraits::StDedxPidTraits(const dst_dedx_st& t) :
    StTrackPidTraits(t),
    mNumberOfPoints(t.ndedx), mDedx(t.dedx[0]),
    mSigma(t.dedx[1]), mMethod(t.method){ /* noop */ }

StDedxPidTraits::~StDedxPidTraits() { /* noop */ }

unsigned short
StDedxPidTraits::numberOfPoints() const { return mNumberOfPoints%100; }
float
StDedxPidTraits::length() const { return (mNumberOfPoints/100); }

float
StDedxPidTraits::mean() const { return mDedx; }

float
StDedxPidTraits::errorOnMean() const { return mSigma; }

StObject*
StDedxPidTraits::clone() const { return new StDedxPidTraits(*this); }

short
StDedxPidTraits::encodedMethod() const { return mMethod; }

StDedxMethod
StDedxPidTraits::method() const
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

