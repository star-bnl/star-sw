#ifndef StPicoEvent_h
#define StPicoEvent_h

#include <vector>
#include "TObject.h"
#include "StarClassLibrary/StThreeVectorF.hh"

class StMuDst;

class StPicoEvent : public TObject
{
public:
  StPicoEvent();
  virtual ~StPicoEvent();
  StPicoEvent(StMuDst const& muDst);

  Int_t    runId() const;
  Int_t    eventId() const;
  Int_t    fillId() const;
  Float_t  bField() const;

  StThreeVectorF const& primaryVertex() const;
  StThreeVectorF const& primaryVertexError() const;
  Float_t               ranking() const;
  UShort_t              nBEMCMatch() const;
  UShort_t              nBTOFMatch() const;

  std::vector<unsigned int> triggerIds() const;
  bool                      isTrigger(unsigned int) const;

  Int_t    refMultPos() const;
  Int_t    refMultNeg() const;
  Int_t    refMultFtpcEast() const;
  Int_t    refMultFtpcWest() const;
  Int_t    refMult() const;
  Int_t    refMultFtpc() const;
  Int_t    refMult2PosEast() const;
  Int_t    refMult2NegEast() const;
  Int_t    refMult2PosWest() const;
  Int_t    refMult2NegWest() const;
  Int_t    refMult3PosEast() const;
  Int_t    refMult3NegEast() const;
  Int_t    refMult3PosWest() const;
  Int_t    refMult3NegWest() const;
  Int_t    refMult4PosEast() const;
  Int_t    refMult4NegEast() const;
  Int_t    refMult4PosWest() const;
  Int_t    refMult4NegWest() const;
  Int_t    refMultHalfPosEast() const;
  Int_t    refMultHalfNegEast() const;
  Int_t    refMultHalfPosWest() const;
  Int_t    refMultHalfNegWest() const;
  Int_t    refMult2East() const;
  Int_t    refMult2West() const;
  Int_t    refMult2() const;
  Int_t    refMultHalfEast() const;
  Int_t    refMultHalfWest() const;
  Int_t    refMult3East() const;
  Int_t    refMult3West() const;
  Int_t    refMult3() const;
  Int_t    refMult4East() const;
  Int_t    refMult4West() const;
  Int_t    refMult4() const;

  Int_t    grefMult() const;
  UShort_t numberOfGlobalTracks() const;
  UShort_t btofTrayMultiplicity() const;
  Int_t    numberOfPxlInnerHits() const;
  Int_t    numberOfPxlOuterHits() const;
  Int_t    numberOfIstHits() const;
  Int_t    numberOfSsdHits() const;

  Int_t    nVpdHitsEast() const;
  Int_t    nVpdHitsWest() const;
  Int_t    nTofT0() const;
  Float_t  vzVpd() const;

  Float_t  ZDCx() const;
  Float_t  BBCx() const;
  Float_t backgroundRate() const;
  Float_t bbcBlueBackgroundRate() const;
  Float_t bbcYellowBackgroundRate() const;
  Float_t bbcEastRate() const;
  Float_t bbcWestRate() const;
  Float_t zdcEastRate() const;
  Float_t zdcWestRate() const;

  Float_t  ZdcSumAdcEast() const;
  Float_t  ZdcSumAdcWest() const;
  Float_t  ZdcSmdEastHorizontal(int i) const;
  Float_t  ZdcSmdEastVertical(int i) const;
  Float_t  ZdcSmdWestHorizontal(int i) const;
  Float_t  ZdcSmdWestVertical(int i) const;

  UShort_t bbcAdcEast(const Int_t i) const;
  UShort_t bbcAdcWest(const Int_t i) const;

  Int_t   highTowerThreshold(const Int_t i) const;
  Int_t   jetPatchThreshold(const Int_t i) const;

  int      year() const;
  int      day() const;

  void     setHighTowerThreshold(const Int_t i, const Int_t th);
  void     setJetPatchThreshold(const Int_t i, const Int_t th);

protected:
  Int_t    mRunId;
  Int_t    mEventId;
  UShort_t mFillId;
  Float_t  mBField;

  StThreeVectorF mPrimaryVertex;
  StThreeVectorF mPrimaryVertexError;
  Float_t mRanking;
  UShort_t mNBEMCMatch;
  UShort_t mNBTOFMatch;

  std::vector<unsigned int> mTriggerIds;

  UShort_t mRefMultFtpcEast;
  UShort_t mRefMultFtpcWest;
  UShort_t mRefMultNeg;
  UShort_t mRefMultPos;
  UShort_t mRefMult2NegEast; // TPC refMult2 neg (-1<eta<-0.5)
  UShort_t mRefMult2PosEast; // TPC refMult2 pos (-1<eta<-0.5)
  UShort_t mRefMult2NegWest; // TPC refMult2 neg (0.5<eta<1.0)
  UShort_t mRefMult2PosWest; // TPC refMult2 pos (0.5<eta<1.0)
  UShort_t mRefMult3NegEast;
  UShort_t mRefMult3PosEast;
  UShort_t mRefMult3NegWest;
  UShort_t mRefMult3PosWest;
  UShort_t mRefMult4NegEast;
  UShort_t mRefMult4PosEast;
  UShort_t mRefMult4NegWest;
  UShort_t mRefMult4PosWest;
  UShort_t mRefMultHalfNegEast ;// TPC refMultHalf neg (eta<0)
  UShort_t mRefMultHalfPosEast ;// TPC refMultHalf pos (eta<0)
  UShort_t mRefMultHalfNegWest ;// TPC refMultHalf neg (eta>0)
  UShort_t mRefMultHalfPosWest ;// TPC refMultHalf pos (eta>0)

  UShort_t mGRefMult;
  UShort_t mNumberOfGlobalTracks ;
  UShort_t mbTofTrayMultiplicity ;
  UShort_t mNHitsHFT[4];

  UChar_t  mNVpdHitsEast;
  UChar_t  mNVpdHitsWest;
  UShort_t mNTofT0;             // number of T0 particles in BTOF self calibration
  Float_t  mVzVpd;

  UInt_t  mZDCx;
  UInt_t  mBBCx;
  Float_t mBackgroundRate;
  Float_t mBbcBlueBackgroundRate;
  Float_t mBbcYellowBackgroundRate;
  Float_t mBbcEastRate;
  Float_t mBbcWestRate;
  Float_t mZdcEastRate;
  Float_t mZdcWestRate;

  UShort_t mZdcSumAdcEast;
  UShort_t mZdcSumAdcWest;
  UShort_t mZdcSmdEastHorizontal[8];
  UShort_t mZdcSmdEastVertical[8];
  UShort_t mZdcSmdWestHorizontal[8];
  UShort_t mZdcSmdWestVertical[8];

  // BBC ADC for q-vectors (Hiroshi)
  UShort_t mBbcAdcEast[24];
  UShort_t mBbcAdcWest[24];

  // Online HT thresholds
  UChar_t mHighTowerThreshold[4];
  // Online JP thresholds BEMC only
  UChar_t mJetPatchThreshold[4];

  ClassDef(StPicoEvent, 1)
};
inline Int_t    StPicoEvent::runId() const { return mRunId; }
inline Int_t    StPicoEvent::eventId() const { return mEventId; }
inline Int_t    StPicoEvent::fillId() const { return (Int_t)mFillId; }
inline Float_t  StPicoEvent::bField() const { return mBField; }

inline StThreeVectorF const & StPicoEvent::primaryVertex() const { return mPrimaryVertex; }
inline StThreeVectorF const & StPicoEvent::primaryVertexError() const { return mPrimaryVertexError; }
inline Float_t  StPicoEvent::ranking() const { return mRanking ; }
inline UShort_t StPicoEvent::nBEMCMatch() const { return mNBEMCMatch ; }
inline UShort_t StPicoEvent::nBTOFMatch() const { return mNBTOFMatch ; }

inline std::vector<unsigned int> StPicoEvent::triggerIds() const { return mTriggerIds; }

inline Int_t    StPicoEvent::refMultPos() const { return (Int_t)mRefMultPos; }
inline Int_t    StPicoEvent::refMultNeg() const { return (Int_t)mRefMultNeg; }
inline Int_t    StPicoEvent::refMultFtpcEast() const { return (Int_t)mRefMultFtpcEast; }
inline Int_t    StPicoEvent::refMultFtpcWest() const { return (Int_t)mRefMultFtpcWest; }
inline Int_t    StPicoEvent::refMult() const { return (Int_t)(mRefMultPos + mRefMultNeg); }
inline Int_t    StPicoEvent::refMultFtpc() const { return (Int_t)(mRefMultFtpcEast + mRefMultFtpcWest); }
inline Int_t    StPicoEvent::refMult2PosEast() const { return (Int_t)mRefMult2PosEast; }
inline Int_t    StPicoEvent::refMult2NegEast() const { return (Int_t)mRefMult2NegEast; }
inline Int_t    StPicoEvent::refMult2PosWest() const { return (Int_t)mRefMult2PosWest; }
inline Int_t    StPicoEvent::refMult2NegWest() const { return (Int_t)mRefMult2NegWest; }
inline Int_t    StPicoEvent::refMult3PosEast() const { return (Int_t)mRefMult3PosEast; }
inline Int_t    StPicoEvent::refMult3NegEast() const { return (Int_t)mRefMult3NegEast; }
inline Int_t    StPicoEvent::refMult3PosWest() const { return (Int_t)mRefMult3PosWest; }
inline Int_t    StPicoEvent::refMult3NegWest() const { return (Int_t)mRefMult3NegWest; }
inline Int_t    StPicoEvent::refMult4PosEast() const { return (Int_t)mRefMult4PosEast; }
inline Int_t    StPicoEvent::refMult4NegEast() const { return (Int_t)mRefMult4NegEast; }
inline Int_t    StPicoEvent::refMult4PosWest() const { return (Int_t)mRefMult4PosWest; }
inline Int_t    StPicoEvent::refMult4NegWest() const { return (Int_t)mRefMult4NegWest; }
inline Int_t    StPicoEvent::refMultHalfPosEast() const { return (Int_t)mRefMultHalfPosEast; }
inline Int_t    StPicoEvent::refMultHalfNegEast() const { return (Int_t)mRefMultHalfNegEast; }
inline Int_t    StPicoEvent::refMultHalfPosWest() const { return (Int_t)mRefMultHalfPosWest; }
inline Int_t    StPicoEvent::refMultHalfNegWest() const { return (Int_t)mRefMultHalfNegWest; }
inline Int_t    StPicoEvent::refMult2East() const { return (Int_t)(mRefMult2PosEast + mRefMult2NegEast); }
inline Int_t    StPicoEvent::refMult2West() const { return (Int_t)(mRefMult2PosWest + mRefMult2NegWest); }
inline Int_t    StPicoEvent::refMult2() const { return (Int_t)(mRefMult2PosEast + mRefMult2NegEast + mRefMult2PosWest + mRefMult2NegWest); }
inline Int_t    StPicoEvent::refMultHalfEast() const { return (Int_t)(mRefMultHalfPosEast + mRefMultHalfNegEast); }
inline Int_t    StPicoEvent::refMultHalfWest() const { return (Int_t)(mRefMultHalfPosWest + mRefMultHalfNegWest); }
inline Int_t    StPicoEvent::refMult3East() const { return (Int_t)(mRefMult3PosEast + mRefMult3NegEast); }
inline Int_t    StPicoEvent::refMult3West() const { return (Int_t)(mRefMult3PosWest + mRefMult3NegWest); }
inline Int_t    StPicoEvent::refMult3() const { return (Int_t)(mRefMult3PosEast + mRefMult3NegEast + mRefMult3PosWest + mRefMult3NegWest); }
inline Int_t    StPicoEvent::refMult4East() const { return (Int_t)(mRefMult4PosEast + mRefMult4NegEast); }
inline Int_t    StPicoEvent::refMult4West() const { return (Int_t)(mRefMult4PosWest + mRefMult4NegWest); }
inline Int_t    StPicoEvent::refMult4() const { return (Int_t)(mRefMult4PosEast + mRefMult4NegEast + mRefMult4PosWest + mRefMult4NegWest); }

inline Int_t    StPicoEvent::grefMult() const { return (Int_t)(mGRefMult); }
inline UShort_t StPicoEvent::numberOfGlobalTracks() const { return mNumberOfGlobalTracks ; }
inline UShort_t StPicoEvent::btofTrayMultiplicity() const { return mbTofTrayMultiplicity ; }
inline Int_t    StPicoEvent::numberOfPxlInnerHits() const { return (Int_t)(mNHitsHFT[0]); }
inline Int_t    StPicoEvent::numberOfPxlOuterHits() const { return (Int_t)(mNHitsHFT[1]); }
inline Int_t    StPicoEvent::numberOfIstHits() const { return (Int_t)(mNHitsHFT[2]); }
inline Int_t    StPicoEvent::numberOfSsdHits() const { return (Int_t)(mNHitsHFT[3]); }

inline Int_t    StPicoEvent::nVpdHitsEast() const { return (Int_t)mNVpdHitsEast; }
inline Int_t    StPicoEvent::nVpdHitsWest() const { return (Int_t)mNVpdHitsWest; }
inline Int_t    StPicoEvent::nTofT0() const { return (Int_t)mNTofT0; }
inline Float_t  StPicoEvent::vzVpd() const { return mVzVpd; }

inline Float_t  StPicoEvent::ZDCx() const { return (Float_t)mZDCx; }
inline Float_t  StPicoEvent::BBCx() const { return (Float_t)mBBCx; }
inline Float_t StPicoEvent::backgroundRate() const { return mBackgroundRate; }
inline Float_t StPicoEvent::bbcBlueBackgroundRate() const { return mBbcBlueBackgroundRate; }
inline Float_t StPicoEvent::bbcYellowBackgroundRate() const { return mBbcYellowBackgroundRate; }
inline Float_t StPicoEvent::bbcEastRate() const { return mBbcEastRate; }
inline Float_t StPicoEvent::bbcWestRate() const { return mBbcWestRate; }
inline Float_t StPicoEvent::zdcEastRate() const { return mZdcEastRate; }
inline Float_t StPicoEvent::zdcWestRate() const { return mZdcWestRate; }

inline Float_t  StPicoEvent::ZdcSumAdcEast() const { return (Float_t)mZdcSumAdcEast; }
inline Float_t  StPicoEvent::ZdcSumAdcWest() const { return (Float_t)mZdcSumAdcWest; }
inline Float_t  StPicoEvent::ZdcSmdEastHorizontal(int i) const { return (Float_t)mZdcSmdEastHorizontal[i]; }
inline Float_t  StPicoEvent::ZdcSmdEastVertical(int i) const { return (Float_t)mZdcSmdEastVertical[i]; }
inline Float_t  StPicoEvent::ZdcSmdWestHorizontal(int i) const { return (Float_t)mZdcSmdWestHorizontal[i]; }
inline Float_t  StPicoEvent::ZdcSmdWestVertical(int i) const { return (Float_t)mZdcSmdWestVertical[i]; }

inline UShort_t StPicoEvent::bbcAdcEast(const Int_t i) const { return mBbcAdcEast[i]; }
inline UShort_t StPicoEvent::bbcAdcWest(const Int_t i) const { return mBbcAdcWest[i]; }

inline Int_t   StPicoEvent::highTowerThreshold(const Int_t i) const { return mHighTowerThreshold[i]; }
inline void StPicoEvent::setHighTowerThreshold(const Int_t i, const Int_t th) { mHighTowerThreshold[i] = (UChar_t)th; }
inline Int_t   StPicoEvent::jetPatchThreshold(const Int_t i) const { return mJetPatchThreshold[i]; }
inline void StPicoEvent::setJetPatchThreshold(const Int_t i, const Int_t th) { mJetPatchThreshold[i] = (UChar_t)th; }
#endif
