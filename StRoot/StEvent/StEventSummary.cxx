/***************************************************************************
 *
 * $Id: StEventSummary.cxx,v 2.11 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventSummary.cxx,v $
 * Revision 2.11  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.10  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.9  2001/05/17 22:56:33  ullrich
 * Removed all usage of dst_summary_param.
 *
 * Revision 2.8  2001/04/05 04:00:49  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.7  2000/01/31 12:01:00  ullrich
 * Unique const_cast syntax for all platforms.
 *
 * Revision 2.6  2000/01/25 15:31:47  fisyak
 * Add namespace for CC5
 *
 * Revision 2.5  2000/01/14 19:06:47  ullrich
 * Made code more robust if read-in table is not well defined.
 *
 * Revision 2.4  2000/01/11 16:11:40  ullrich
 * Magnetic field in kGauss.
 *
 * Revision 2.3  1999/12/21 15:08:50  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:25:13  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:35  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include <float.h>
#include "StEventSummary.h"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using units::tesla;
using units::degree;
#endif

static const char rcsid[] = "$Id: StEventSummary.cxx,v 2.11 2009/11/23 16:34:06 fisyak Exp $";

ClassImp(StEventSummary)

StEventSummary::StEventSummary()
{
    initBinRanges();
    
    mNumberOfTracks = 0;
    mNumberOfGoodTracks = 0;
    mNumberOfGoodPrimaryTracks = 0;
    mNumberOfPositiveTracks = 0;
    mNumberOfNegativeTracks = 0;
    mNumberOfExoticTracks = 0;
    mNumberOfVertices = 0;
    mNumberOfVertexTypes.Set(mVertexTypeArraySize);
    mNumberOfPileupVertices = 0;
    mMeanPt = 0;
    mMeanPt2 = 0;
    mMeanEta = 0;
    mRmsEta = 0;
    
    mEtaBins.Set(mPtAndEtaBinsSize);
    mPtBins.Set(mPtAndEtaBinsSize);
    mPhiBins.Set(mPhiBinsSize);

    mEtaOfTracksHisto.Set(mHistogramSize);
    mPtOfTracksHisto.Set(mHistogramSize);
    mPhiOfTracksHisto.Set(mHistogramSize);
    mEneryVsEtaHisto.Set(mHistogramSize);
    mEnergyVsPhiHisto.Set(mHistogramSize);
    mMagneticFieldZ = 0;
}


StEventSummary::~StEventSummary() { /* noop */ }

void
StEventSummary::initBinRanges()
{
    mVertexTypeArraySize = 5;
    mPhiBinsSize         = 10;
    mPtAndEtaBinsSize    = 9;
    mHistogramSize       = 10;
    
    mEtaBins.Set(mPtAndEtaBinsSize);
    mPtBins.Set(mPtAndEtaBinsSize);
    mPhiBins.Set(mPhiBinsSize);
        
    mEtaBins[0] = -2.;
    mEtaBins[1] = -1.;
    mEtaBins[2] = -0.5;
    mEtaBins[3] =  .5;
    mEtaBins[4] =  1.;
    mEtaBins[5] =  2.;
    mEtaBins[6] =  2.;
    mEtaBins[7] =  2.;
    mEtaBins[8] =  2.;
    
    mPtBins[0] = 0.1;
    mPtBins[1] = 0.15;
    mPtBins[2] = 0.2;
    mPtBins[3] = 0.3;
    mPtBins[4] = 0.5;
    mPtBins[5] = 1.;
    mPtBins[6] = 1.;
    mPtBins[7] = 1.;
    mPtBins[8] = 1.;
    
    mPhiBins[0] = 0*degree;
    mPhiBins[1] = 36*degree;
    mPhiBins[2] = 72*degree;
    mPhiBins[3] = 108*degree;
    mPhiBins[4] = 144*degree;
    mPhiBins[5] = 180*degree;
    mPhiBins[6] = 216*degree;
    mPhiBins[7] = 252*degree;
    mPhiBins[8] = 288*degree;
    mPhiBins[9] = 324*degree;
}

int StEventSummary::numberOfTracks() const { return mNumberOfTracks; }

int StEventSummary::numberOfGoodTracks() const { return mNumberOfGoodTracks; }

int StEventSummary::numberOfGoodTracks(StChargeSign s) const
{
    return s == negative ? mNumberOfNegativeTracks : mNumberOfPositiveTracks;
}

int
StEventSummary::numberOfGoodPrimaryTracks() const { return mNumberOfGoodPrimaryTracks; }

int
StEventSummary::numberOfExoticTracks() const { return mNumberOfExoticTracks; }

int
StEventSummary::numberOfVertices() const { return mNumberOfVertices; }

int
StEventSummary::numberOfVerticesOfType(StVertexId id) const
{
    unsigned int i = id-1;        // vector numbering scheme starts at 1
    if (i < mVertexTypeArraySize)
        return const_cast<TArrayL&>(mNumberOfVertexTypes)[i];
    else
        return 0;
}

int
StEventSummary::numberOfPileupVertices() const { return mNumberOfPileupVertices; }

float
StEventSummary::meanPt() const { return mMeanPt; }

float
StEventSummary::meanPt2() const { return mMeanPt2; }

float
StEventSummary::meanEta() const { return mMeanEta; }

float
StEventSummary::rmsEta() const { return mRmsEta; }

const StThreeVectorF&
StEventSummary::primaryVertexPosition() const { return mPrimaryVertexPos; }

unsigned int
StEventSummary::numberOfBins() const { return mHistogramSize; }

int
StEventSummary::tracksInEtaBin(unsigned int i) const
{
        return i < mHistogramSize ? const_cast<TArrayL&>(mEtaOfTracksHisto)[i] : 0;
}

int
StEventSummary::tracksInPhiBin(unsigned int i) const
{
        return i < mHistogramSize ? const_cast<TArrayL&>(mPhiOfTracksHisto)[i] : 0;
}

int
StEventSummary::tracksInPtBin(unsigned int i) const
{
        return i < mHistogramSize ? const_cast<TArrayL&>(mPtOfTracksHisto)[i] : 0;
}

float
StEventSummary::energyInEtaBin(unsigned int i) const
{
        return i < mHistogramSize ? const_cast<TArrayF&>(mEneryVsEtaHisto)[i] : 0;
}

float
StEventSummary::energyInPhiBin(unsigned int i) const
{
        return i < mHistogramSize ? const_cast<TArrayF&>(mEnergyVsPhiHisto)[i] : 0;
}

float
StEventSummary::lowerEdgeEtaBin(unsigned int i) const
{
    if (i <= mPtAndEtaBinsSize) {
        if (i == 0)
            return -FLT_MAX;     // no lower limit
        else
            return const_cast<TArrayF&>(mEtaBins)[i-1];
    }
    else
        return 0;
}

float
StEventSummary::upperEdgeEtaBin(unsigned int i) const
{
    if (i <= mPtAndEtaBinsSize) {
       if (i == mPtAndEtaBinsSize)
           return FLT_MAX;
       else
           return const_cast<TArrayF&>(mEtaBins)[i];
    }
    else
        return 0;
}

float
StEventSummary::lowerEdgePtBin(unsigned int i) const
{
    if (i <= mPtAndEtaBinsSize) {
        if (i == 0)
            return 0;
        else
            return const_cast<TArrayF&>(mPtBins)[i-1];
    }
    else
        return 0;
}

float
StEventSummary::upperEdgePtBin(unsigned int i) const
{
    if (i <= mPtAndEtaBinsSize) {
       if (i == mPtAndEtaBinsSize)
           return FLT_MAX;
       else
           return const_cast<TArrayF&>(mPtBins)[i];
    }
    else
        return 0;
}

float
StEventSummary::lowerEdgePhiBin(unsigned int i) const
{
    if (i < mPhiBinsSize)
        return const_cast<TArrayF&>(mPhiBins)[i];
    else
        return 0;
}

float
StEventSummary::upperEdgePhiBin(unsigned int i) const
{
    if (i < mPhiBinsSize) {
       if ((int)i == mPhiBinsSize-1)
           return const_cast<TArrayF&>(mPhiBins)[0];
       else
           return const_cast<TArrayF&>(mPhiBins)[i+1];
    }
    else
        return 0;
}

void
StEventSummary::setNumberOfTracks(int val) { mNumberOfTracks = val; }

void
StEventSummary::setNumberOfGoodTracks(int val) { mNumberOfGoodTracks = val; }

void
StEventSummary::setNumberOfGoodTracks(StChargeSign s, int val)
{
    if (s == negative)
        mNumberOfNegativeTracks = val;
    else
        mNumberOfPositiveTracks = val;
}

void
StEventSummary::setNumberOfGoodPrimaryTracks(int val) { mNumberOfGoodPrimaryTracks = val; }

void
StEventSummary::setNumberOfExoticTracks( int val) { mNumberOfExoticTracks = val; }

void
StEventSummary::setNumberOfVertices(int val) { mNumberOfVertices = val; }

void
StEventSummary::setNumberOfVerticesForType(StVertexId id, int val)
{
    int i = id-1;
    if (i < mVertexTypeArraySize)
        mNumberOfVertexTypes[i] = val;
}

void
StEventSummary::setNumberOfPileupVertices(int val) { mNumberOfPileupVertices = val; }

void
StEventSummary::setMeanPt(float val) { mMeanPt = val; }

void
StEventSummary::setMeanPt2(float val) { mMeanPt2 = val; }

void
StEventSummary::setMeanEta(float val) { mMeanEta = val; }

void
StEventSummary::setRmsEta(float val) { mRmsEta = val; }

void
StEventSummary::setPrimaryVertexPosition(const StThreeVectorF& val) { mPrimaryVertexPos = val; }

double
StEventSummary::magneticField() const { return mMagneticFieldZ; }

void
StEventSummary::setMagneticField(double val) { mMagneticFieldZ = val; }
