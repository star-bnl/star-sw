/***************************************************************************
 *
 * $Id: StEventSummary.cxx,v 2.4 2000/01/11 16:11:40 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventSummary.cxx,v $
 * Revision 2.4  2000/01/11 16:11:40  ullrich
 * Magnetic field in kGauss.
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
#include "tables/St_dst_event_summary_Table.h"
#ifndef ST_NO_NAMESPACES
using units::tesla;
static const char rcsid[] = "$Id: StEventSummary.cxx,v 2.4 2000/01/11 16:11:40 ullrich Exp $";
#endif

static const char rcsid[] = "$Id: StEventSummary.cxx,v 2.4 2000/01/11 16:11:40 ullrich Exp $";

ClassImp(StEventSummary)

StEventSummary::StEventSummary()
{
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

{
    int i;
    
    mNumberOfTracks            = runSum.glb_trk_tot;
    mNumberOfGoodTracks        = runSum.glb_trk_good;
    mNumberOfGoodPrimaryTracks = runSum.glb_trk_prim;
    mNumberOfPositiveTracks    = runSum.glb_trk_plus;
    mNumberOfVertexTypes.Set(mVertexTypeArraySize,
                             const_cast<long*>(runSum.n_vert_type));
    mNumberOfNegativeTracks    = runSum.glb_trk_minus;
    mNumberOfExoticTracks      = runSum.glb_trk_exotic;
    mNumberOfVertices          = runSum.n_vert_total;
    mNumberOfPileupVertices    = runSum.n_vert_pileup;
    mMeanPt                    = runSum.mean_pt;
    mMeanPt2                   = runSum.mean_pt2;
    mRmsEta                    = runSum.rms_eta;
    mEtaBins.Set(mPtAndEtaBinsSize, const_cast<float*>(sumPar.eta_bins));
    mPtBins.Set(mPtAndEtaBinsSize, const_cast<float*>(sumPar.pt_bins));
    mPhiBins.Set(mPhiBinsSize, const_cast<float*>(sumPar.phi_bins));

    mEtaOfTracksHisto.Set(mHistogramSize, const_cast<long*>(runSum.mult_eta));
    mPtOfTracksHisto.Set(mHistogramSize, const_cast<long*>(runSum.mult_pt));
    mPhiOfTracksHisto.Set(mHistogramSize, const_cast<long*>(runSum.mult_phi));
    mEneryVsEtaHisto.Set(mHistogramSize, const_cast<float*>(runSum.energy_emc_eta));
    mEnergyVsPhiHisto.Set(mHistogramSize, const_cast<float*>(runSum.energy_emc_phi));

    mMagneticFieldZ = runSum.field;
	mEneryVsEtaHisto[i] = runSum.energy_emc_eta[i];
	mEnergyVsPhiHisto[i] = runSum.energy_emc_phi[i];
    }
}

StEventSummary::~StEventSummary() { /* noop */ }
    
Long_t StEventSummary::numberOfTracks() const { return mNumberOfTracks; }

Long_t StEventSummary::numberOfGoodTracks() const { return mNumberOfGoodTracks; }

Long_t StEventSummary::numberOfGoodTracks(StChargeSign s) const
{
    return s == negative ? mNumberOfNegativeTracks : mNumberOfPositiveTracks;
}

Long_t
StEventSummary::numberOfGoodPrimaryTracks() const { return mNumberOfGoodPrimaryTracks; }

Long_t
StEventSummary::numberOfExoticTracks() const { return mNumberOfExoticTracks; }

Long_t
StEventSummary::numberOfVertices() const { return mNumberOfVertices; }

Long_t
StEventSummary::numberOfVerticesOfType(StVertexId id) const
{
    unsigned int i = id-1;        // vector numbering scheme starts at 1
    if (i < mVertexTypeArraySize) {
#if defined(__SUNPRO_CC)
        return (*(const_cast<TArrayL*>(&mNumberOfVertexTypes)))[i];
#else
        return const_cast<TArrayL>(mNumberOfVertexTypes)[i];
#endif
    }
    else
        return 0;
}

Long_t
StEventSummary::numberOfPileupVertices() const { return mNumberOfPileupVertices; }

Float_t
StEventSummary::meanPt() const { return mMeanPt; }

Float_t
StEventSummary::meanPt2() const { return mMeanPt2; }

Float_t
StEventSummary::meanEta() const { return mMeanEta; }

Float_t
StEventSummary::rmsEta() const { return mRmsEta; }

const StThreeVectorF&
StEventSummary::primaryVertexPosition() const { return mPrimaryVertexPos; }

UInt_t
StEventSummary::numberOfBins() const { return mHistogramSize; }

Long_t
StEventSummary::tracksInEtaBin(UInt_t i) const
{
#if defined(__SUNPRO_CC)
        return i < mHistogramSize ? (*(const_cast<TArrayL*>(&mEtaOfTracksHisto)))[i] : 0;
#else
        return i < mHistogramSize ? const_cast<TArrayL>(mEtaOfTracksHisto)[i] : 0;
#endif
}

Long_t
StEventSummary::tracksInPhiBin(UInt_t i) const
{
#if defined(__SUNPRO_CC)
        return i < mHistogramSize ? (*(const_cast<TArrayL*>(&mPhiOfTracksHisto)))[i] : 0;
#else
        return i < mHistogramSize ? const_cast<TArrayL>(mPhiOfTracksHisto)[i] : 0;
#endif
}

Long_t
StEventSummary::tracksInPtBin(UInt_t i) const
{
#if defined(__SUNPRO_CC)
        return i < mHistogramSize ? (*(const_cast<TArrayL*>(&mPtOfTracksHisto)))[i] : 0;
#else
        return i < mHistogramSize ? const_cast<TArrayL>(mPtOfTracksHisto)[i] : 0;
#endif
}

Float_t
StEventSummary::energyInEtaBin(UInt_t i) const
{
#if defined(__SUNPRO_CC)
        return i < mHistogramSize ? (*(const_cast<TArrayF*>(&mEneryVsEtaHisto)))[i] : 0;
#else
        return i < mHistogramSize ? const_cast<TArrayF>(mEneryVsEtaHisto)[i] : 0;
#endif
}

Float_t
StEventSummary::energyInPhiBin(UInt_t i) const
{
#if defined(__SUNPRO_CC)
        return i < mHistogramSize ? (*(const_cast<TArrayF*>(&mEnergyVsPhiHisto)))[i] : 0;
#else
        return i < mHistogramSize ? const_cast<TArrayF>(mEnergyVsPhiHisto)[i] : 0;
#endif
}

    if (i < mPtAndEtaBinsSize) {
StEventSummary::lowerEdgeEtaBin(UInt_t i) const
{
    if (i <= mPtAndEtaBinsSize) {
        if (i == 0)
            return -FLT_MAX;     // no lower limit
        else
#if defined(__SUNPRO_CC)
            return (*(const_cast<TArrayF*>(&mEtaBins)))[i-1];
#else
            return const_cast<TArrayF>(mEtaBins)[i-1];
#endif
    }
    else
        return 0;
}

    if (i < mPtAndEtaBinsSize) {
       if (i == mPtAndEtaBinsSize-1)
           return FLT_MAX;       // no upper limit
    if (i <= mPtAndEtaBinsSize) {
       if (i == mPtAndEtaBinsSize)
           return FLT_MAX;
       else
#if defined(__SUNPRO_CC)
           return (*(const_cast<TArrayF*>(&mEtaBins)))[i];
#else
           return const_cast<TArrayF>(mEtaBins)[i];
#endif
    }
    else
        return 0;
}

    if (i < mPtAndEtaBinsSize) {
StEventSummary::lowerEdgePtBin(UInt_t i) const
{
    if (i <= mPtAndEtaBinsSize) {
        if (i == 0)
            return 0;
        else
#if defined(__SUNPRO_CC)
            return (*(const_cast<TArrayF*>(&mPtBins)))[i-1];
#else
            return const_cast<TArrayF>(mPtBins)[i-1];
#endif
    }
    else
        return 0;
}

    if (i < mPtAndEtaBinsSize) {
       if (i == mPtAndEtaBinsSize-1)
           return FLT_MAX;       // no upper limit
    if (i <= mPtAndEtaBinsSize) {
       if (i == mPtAndEtaBinsSize)
           return FLT_MAX;
       else
#if defined(__SUNPRO_CC)
           return (*(const_cast<TArrayF*>(&mPtBins)))[i];
#else
           return const_cast<TArrayF>(mPtBins)[i];
#endif
    }
    else
        return 0;
}

Float_t
StEventSummary::lowerEdgePhiBin(UInt_t i) const
{
    if (i < mPhiBinsSize)
#if defined(__SUNPRO_CC)
        return (*(const_cast<TArrayF*>(&mPhiBins)))[i];
#else
        return const_cast<TArrayF>(mPhiBins)[i];
#endif
    else
        return 0;
}

Float_t
StEventSummary::upperEdgePhiBin(UInt_t i) const
{
    if (i < mPhiBinsSize) {
       if (i == mPhiBinsSize-1)
#if defined(__SUNPRO_CC)
           return (*(const_cast<TArrayF*>(&mPhiBins)))[0];
#else
           return const_cast<TArrayF>(mPhiBins)[0];
#endif
       else
#if defined(__SUNPRO_CC)
           return (*(const_cast<TArrayF*>(&mPhiBins)))[i+1];
#else
           return const_cast<TArrayF>(mPhiBins)[i+1];
#endif
    }
    else
        return 0;
}

void
StEventSummary::setNumberOfTracks(Long_t val) { mNumberOfTracks = val; }

void
StEventSummary::setNumberOfGoodTracks(Long_t val) { mNumberOfGoodTracks = val; }

void
StEventSummary::setNumberOfGoodTracks(StChargeSign s, Long_t val)
{
    if (s == negative)
        mNumberOfNegativeTracks = val;
    else
        mNumberOfPositiveTracks = val;
}

void
StEventSummary::setNumberOfGoodPrimaryTracks(Long_t val) { mNumberOfGoodPrimaryTracks = val; }

void
StEventSummary::setNumberOfExoticTracks( Long_t val) { mNumberOfExoticTracks = val; }

void
StEventSummary::setNumberOfVertices(Long_t val) { mNumberOfVertices = val; }

void
StEventSummary::setNumberOfVerticesForType(StVertexId id, Long_t val)
{
    int i = id-1;
    if (i < mVertexTypeArraySize)
        mNumberOfVertexTypes[i] = val;
}

void
StEventSummary::setNumberOfPileupVertices(Long_t val) { mNumberOfPileupVertices = val; }

void
StEventSummary::setMeanPt(Float_t val) { mMeanPt = val; }

void
StEventSummary::setMeanPt2(Float_t val) { mMeanPt2 = val; }

void
StEventSummary::setMeanEta(Float_t val) { mMeanEta = val; }

void
StEventSummary::setRmsEta(Float_t val) { mRmsEta = val; }

void
StEventSummary::setPrimaryVertexPosition(const StThreeVectorF& val) { mPrimaryVertexPos = val; }

Double_t
StEventSummary::magneticField() const { return mMagneticFieldZ; }

void
StEventSummary::setMagneticField(Double_t val) { mMagneticFieldZ = val; }
