/***************************************************************************
 *
 * $Id: StEventSummary.h,v 2.1 1999/10/13 19:43:02 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventSummary.h,v $
 * Revision 2.1  1999/10/13 19:43:02  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEventSummary_hh
#define StEventSummary_hh

#include <iostream.h>
#include "StObject.h"
#include "StEnumerations.h"
#include "StThreeVectorF.hh"
#include "TString.h"
#include "TArrayF.h"
#include "TArrayL.h"

class dst_event_summary_st;
class dst_summary_param_st;

class StEventSummary : public StObject {
public:
    StEventSummary();
    StEventSummary(const dst_event_summary_st&,
                   const dst_summary_param_st&);
    virtual ~StEventSummary();
    
    // StEventSummary& operator=(const StEventSummary&); use default
    // StEventSummary(const StEventSummary&);            use default
    
    Long_t   numberOfTracks() const;
    Long_t   numberOfGoodTracks() const;
    Long_t   numberOfGoodTracks(StChargeSign) const;
    Long_t   numberOfGoodPrimaryTracks() const;
    Long_t   numberOfExoticTracks() const;
    Long_t   numberOfVertices() const;
    Long_t   numberOfVerticesOfType(StVertexId) const;
    Long_t   numberOfPileupVertices() const;
    Float_t  meanPt() const;
    Float_t  meanPt2() const;
    Float_t  meanEta() const;
    Float_t  rmsEta() const;
    const StThreeVectorF& primaryVertexPosition() const;

    UInt_t   numberOfBins() const;
    Long_t   tracksInEtaBin(UInt_t) const;
    Long_t   tracksInPhiBin(UInt_t) const;
    Long_t   tracksInPtBin(UInt_t) const;
    Float_t  energyInEtaBin(UInt_t) const;
    Float_t  energyInPhiBin(UInt_t) const;

    Float_t  lowerEdgeEtaBin(UInt_t) const;
    Float_t  upperEdgeEtaBin(UInt_t) const;
    Float_t  lowerEdgePhiBin(UInt_t) const;
    Float_t  upperEdgePhiBin(UInt_t) const;
    Float_t  lowerEdgePtBin(UInt_t) const;
    Float_t  upperEdgePtBin(UInt_t) const;
    Double_t magneticField()        const;
    
    void setNumberOfTracks(Long_t);
    void setNumberOfGoodTracks(Long_t);
    void setNumberOfGoodTracks(StChargeSign, Long_t);
    void setNumberOfGoodPrimaryTracks(Long_t);
    void setNumberOfNegativeTracks(Long_t);
    void setNumberOfExoticTracks(Long_t);
    void setNumberOfVertices(Long_t);
    
    void setNumberOfVerticesForType(StVertexId, Long_t);
    void setNumberOfPileupVertices(Long_t);
    void setMeanPt(Float_t);
    void setMeanPt2(Float_t);
    void setMeanEta(Float_t);
    void setRmsEta(Float_t);
    void setPrimaryVertexPosition(const StThreeVectorF&);
    void setMagneticField(Double_t);

protected:
    enum { mVertexTypeArraySize = 5,
           mPhiBinsSize = 10,
           mPtAndEtaBinsSize = 9,
           mHistogramSize = 10 };
    
    Long_t         mNumberOfTracks;
    Long_t         mNumberOfGoodTracks;
    Long_t         mNumberOfGoodPrimaryTracks;
    Long_t         mNumberOfPositiveTracks;
    Long_t         mNumberOfNegativeTracks;
    Long_t         mNumberOfExoticTracks;
    Long_t         mNumberOfVertices;
    TArrayL        mNumberOfVertexTypes;
    Long_t         mNumberOfPileupVertices;
    Float_t        mMeanPt;
    Float_t        mMeanPt2;
    Float_t        mMeanEta;
    Float_t        mRmsEta;
    StThreeVectorF mPrimaryVertexPos;

    TArrayF        mEtaBins;
    TArrayF        mPtBins;
    TArrayF        mPhiBins;

    TArrayL        mEtaOfTracksHisto;
    TArrayL        mPtOfTracksHisto;
    TArrayL        mPhiOfTracksHisto;
    TArrayF        mEneryVsEtaHisto;
    TArrayF        mEnergyVsPhiHisto;
    Double_t       mMagneticFieldZ;
    
    ClassDef(StEventSummary,1)
};

#endif
