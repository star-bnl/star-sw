/***************************************************************************
 *
 * $Id: StEventSummary.cxx,v 2.7 2000/01/31 12:01:00 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventSummary.cxx,v $
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
#include "tables/St_dst_event_summary_Table.h"
#include "tables/St_dst_summary_param_Table.h"
#ifndef ST_NO_NAMESPACES
using units::tesla;
using units::degree;
#endif

static const char rcsid[] = "$Id: StEventSummary.cxx,v 2.7 2000/01/31 12:01:00 ullrich Exp $";

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

StEventSummary::StEventSummary(const dst_event_summary_st& runSum,
                               const dst_summary_param_st& sumPar)
{
    int i;
    
    mNumberOfTracks            = runSum.glb_trk_tot;
    mNumberOfGoodTracks        = runSum.glb_trk_good;
    mNumberOfGoodPrimaryTracks = runSum.glb_trk_prim;
    mNumberOfPositiveTracks    = runSum.glb_trk_plus;
    mNumberOfNegativeTracks    = runSum.glb_trk_minus;
    mNumberOfExoticTracks      = runSum.glb_trk_exotic;
    mNumberOfVertices          = runSum.n_vert_total;
    mNumberOfPileupVertices    = runSum.n_vert_pileup;
    mMeanPt                    = runSum.mean_pt;
    mMeanPt2                   = runSum.mean_pt2;
    mMeanEta                   = runSum.mean_eta;
    mRmsEta                    = runSum.rms_eta;
    mPrimaryVertexPos          = StThreeVectorF(runSum.prim_vrtx);
    mMagneticFieldZ            = runSum.field;

    //
    //   Vertex counts
    //
    mNumberOfVertexTypes.Set(mVertexTypeArraySize);
    for(i=0; i<mVertexTypeArraySize; i++)
	mNumberOfVertexTypes[i] = runSum.n_vert_type[i];
    
    //
    //   Set the 'histo' bins
    //
    mEtaBins.Set(mPtAndEtaBinsSize);
    mPtBins.Set(mPtAndEtaBinsSize);
    mPhiBins.Set(mPhiBinsSize);

    for(i=0; i<mPtAndEtaBinsSize; i++) {
	mEtaBins[i] = sumPar.eta_bins[i];
	mPtBins[i] = sumPar.pt_bins[i];
    }
    for(i=0; i<mPhiBinsSize; i++) mPhiBins[i] = sumPar.phi_bins[i]*degree;
    
    //
    //   Fill 'histos'
    //
    mEtaOfTracksHisto.Set(mHistogramSize);
    mPtOfTracksHisto.Set(mHistogramSize);
    mPhiOfTracksHisto.Set(mHistogramSize);
    mEneryVsEtaHisto.Set(mHistogramSize);
    mEnergyVsPhiHisto.Set(mHistogramSize);

    for(i=0; i<mHistogramSize; i++) {
	mEtaOfTracksHisto[i] = runSum.mult_eta[i];
	mPtOfTracksHisto[i] = runSum.mult_pt[i];
	mPhiOfTracksHisto[i] = runSum.mult_phi[i];
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
    if (i < mVertexTypeArraySize) 
        return const_cast<TArrayL&>(mNumberOfVertexTypes)[i];
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
        return i < mHistogramSize ? const_cast<TArrayL&>(mEtaOfTracksHisto)[i] : 0;
}

Long_t
StEventSummary::tracksInPhiBin(UInt_t i) const
{
        return i < mHistogramSize ? const_cast<TArrayL&>(mPhiOfTracksHisto)[i] : 0;
}

Long_t
StEventSummary::tracksInPtBin(UInt_t i) const
{
        return i < mHistogramSize ? const_cast<TArrayL&>(mPtOfTracksHisto)[i] : 0;
}

Float_t
StEventSummary::energyInEtaBin(UInt_t i) const
{
        return i < mHistogramSize ? const_cast<TArrayF&>(mEneryVsEtaHisto)[i] : 0;
}

Float_t
StEventSummary::energyInPhiBin(UInt_t i) const
{
        return i < mHistogramSize ? const_cast<TArrayF&>(mEnergyVsPhiHisto)[i] : 0;
}

Float_t
StEventSummary::lowerEdgeEtaBin(UInt_t i) const
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

Float_t
StEventSummary::upperEdgeEtaBin(UInt_t i) const
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

Float_t
StEventSummary::lowerEdgePtBin(UInt_t i) const
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

Float_t
StEventSummary::upperEdgePtBin(UInt_t i) const
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

Float_t
StEventSummary::lowerEdgePhiBin(UInt_t i) const
{
    if (i < mPhiBinsSize)
        return const_cast<TArrayF&>(mPhiBins)[i];
    else
        return 0;
}

Float_t
StEventSummary::upperEdgePhiBin(UInt_t i) const
{
    if (i < mPhiBinsSize) {
       if (i == mPhiBinsSize-1)
           return const_cast<TArrayF&>(mPhiBins)[0];
       else
           return const_cast<TArrayF&>(mPhiBins)[i+1];
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
