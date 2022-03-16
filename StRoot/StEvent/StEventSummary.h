/*!
 * \class StEventSummary 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StEventSummary.h,v 2.9 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEventSummary.h,v $
 * Revision 2.9  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.8  2007/10/25 19:12:58  ullrich
 * Removed obsolete method setNumberOfNegativeTracks().
 *
 * Revision 2.7  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.6  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.5  2001/05/30 17:45:54  perev
 * StEvent branching
 *
 * Revision 2.4  2001/05/17 22:56:33  ullrich
 * Removed all usage of dst_summary_param.
 *
 * Revision 2.3  2001/04/05 04:00:36  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  2000/01/14 19:06:51  ullrich
 * Made code more robust if read-in table is not well defined.
 *
 * Revision 2.1  1999/10/13 19:43:02  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEventSummary_hh
#define StEventSummary_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StEnumerations.h"
#include "StThreeVectorF.hh"
#include "TString.h"
#include "TArrayF.h"
#include "TArrayL.h"

class StEventSummary : public StObject {
public:
    StEventSummary();
    virtual ~StEventSummary();
    
    // StEventSummary& operator=(const StEventSummary&); use default
    // StEventSummary(const StEventSummary&);            use default
    
    int    numberOfTracks() const;
    int    numberOfGoodTracks() const;
    int    numberOfGoodTracks(StChargeSign) const;
    int    numberOfGoodPrimaryTracks() const;
    int    numberOfExoticTracks() const;
    int    numberOfVertices() const;
    int    numberOfVerticesOfType(StVertexId) const;
    int    numberOfPileupVertices() const;
    float  meanPt() const;
    float  meanPt2() const;
    float  meanEta() const;
    float  rmsEta() const;
    double magneticField() const;
    
    const StThreeVectorF& primaryVertexPosition() const;

    unsigned int   numberOfBins() const;
    int    tracksInEtaBin(unsigned int) const;
    int    tracksInPhiBin(unsigned int) const;
    int    tracksInPtBin(unsigned int) const;
    float  energyInEtaBin(unsigned int) const;
    float  energyInPhiBin(unsigned int) const;

    float  lowerEdgeEtaBin(unsigned int) const;
    float  upperEdgeEtaBin(unsigned int) const;
    float  lowerEdgePhiBin(unsigned int) const;
    float  upperEdgePhiBin(unsigned int) const;
    float  lowerEdgePtBin(unsigned int) const;
    float  upperEdgePtBin(unsigned int) const;
    
    void setNumberOfTracks(int);
    void setNumberOfGoodTracks(int);
    void setNumberOfGoodTracks(StChargeSign, int);
    void setNumberOfGoodPrimaryTracks(int);
    void setNumberOfExoticTracks(int);
    void setNumberOfVertices(int);
    
    void setNumberOfVerticesForType(StVertexId, int);
    void setNumberOfPileupVertices(int);
    void setMeanPt(float);
    void setMeanPt2(float);
    void setMeanEta(float);
    void setRmsEta(float);
    void setPrimaryVertexPosition(const StThreeVectorF&);
    void setMagneticField(double);

protected:
    void initBinRanges();
    
protected:
    Int_t          mNumberOfTracks;
    Int_t          mNumberOfGoodTracks;
    Int_t          mNumberOfGoodPrimaryTracks;
    Int_t          mNumberOfPositiveTracks;
    Int_t          mNumberOfNegativeTracks;
    Int_t          mNumberOfExoticTracks;
    Int_t          mNumberOfVertices;
    TArrayL        mNumberOfVertexTypes;
    Int_t          mNumberOfPileupVertices;
    Float_t        mMeanPt;
    Float_t        mMeanPt2;
    Float_t        mMeanEta;
    Float_t        mRmsEta;
    StThreeVectorF mPrimaryVertexPos;

    UShort_t       mVertexTypeArraySize;        
    UShort_t       mPhiBinsSize;        
    UShort_t       mPtAndEtaBinsSize;        
    UShort_t       mHistogramSize;        
    TArrayF        mEtaBins;
    TArrayF        mPtBins;
    TArrayF        mPhiBins;

    TArrayL        mEtaOfTracksHisto;
    TArrayL        mPtOfTracksHisto;
    TArrayL        mPhiOfTracksHisto;
    TArrayF        mEneryVsEtaHisto;
    TArrayF        mEnergyVsPhiHisto;
    Double_t       mMagneticFieldZ;
    
    ClassDef(StEventSummary,2)
};

#endif
