#ifndef StPicoEvent_hh
#define StPicoEvent_hh

class StMuDst;
//class StMuEvent;
class TClonesArray;
class StMuPrimaryVertex;
class StPicoDst;
class StPicoDstMaker;
class StBTofHeader;
#include "StThreeVectorF.hh"
#include "TVector2.h"

class StPicoEvent : public TObject {
public:
  StPicoEvent();
  ~StPicoEvent();
  StPicoEvent(const StMuDst& muDst, const Float_t* Q) ;
//  StPicoEvent(StMuEvent*, StBTofHeader*, float*);
  void Clear(const Option_t*) {}
  
  Int_t    runId() const            { return mRunId; }
  Int_t    eventId() const          { return mEventId; }
  Int_t    fillId() const           { return (Int_t)mFillId; }
  Float_t  bField() const           { return mBField; }
  StThreeVectorF const & primaryVertex() const { return mPrimaryVertex; }
  StThreeVectorF const & primaryVertexError() const { return mPrimaryVertexError; }
  Int_t    triggerWord() const      { return mTriggerWord; }
  Int_t    triggerWordMtd() const   { return mTriggerWordMtd; }
  Int_t    refMultPos() const       { return (Int_t)mRefMultPos; }
  Int_t    refMultNeg() const       { return (Int_t)mRefMultNeg; }
  Int_t    refMultFtpcEast() const  { return (Int_t)mRefMultFtpcEast; }
  Int_t    refMultFtpcWest() const  { return (Int_t)mRefMultFtpcWest; }
  Int_t    refMult() const          { return (Int_t)(mRefMultPos+mRefMultNeg); }
  Int_t    refMultFtpc() const      { return (Int_t)(mRefMultFtpcEast+mRefMultFtpcWest); }
  Int_t    refMult2PosEast() const      { return (Int_t)mRefMult2PosEast; }
  Int_t    refMult2NegEast() const      { return (Int_t)mRefMult2NegEast; }
  Int_t    refMult2PosWest() const      { return (Int_t)mRefMult2PosWest; }
  Int_t    refMult2NegWest() const      { return (Int_t)mRefMult2NegWest; }
  Int_t    refMultHalfPosEast() const { return (Int_t)mRefMultHalfPosEast; }
  Int_t    refMultHalfNegEast() const { return (Int_t)mRefMultHalfNegEast; }
  Int_t    refMultHalfPosWest() const { return (Int_t)mRefMultHalfPosWest; }
  Int_t    refMultHalfNegWest() const { return (Int_t)mRefMultHalfNegWest; }
  Int_t    refMult2East() const       { return (Int_t)(mRefMult2PosEast+mRefMult2NegEast); }
  Int_t    refMult2West() const       { return (Int_t)(mRefMult2PosWest+mRefMult2NegWest); }
  Int_t    refMult2() const           { return (Int_t)(mRefMult2PosEast+mRefMult2NegEast+mRefMult2PosWest+mRefMult2NegWest); }
  Int_t    refMultHalfEast() const    { return (Int_t)(mRefMultHalfPosEast+mRefMultHalfNegEast); }
  Int_t    refMultHalfWest() const    { return (Int_t)(mRefMultHalfPosWest+mRefMultHalfNegWest); }
  Int_t    grefMult() const           { return (Int_t)(mGRefMult); }
  Int_t    numberOfPxlInnerHits() const { return (Int_t)(mNHitsHFT[0]); }
  Int_t    numberOfPxlOuterHits() const { return (Int_t)(mNHitsHFT[1]); }
  Int_t    numberOfIstHits() const      { return (Int_t)(mNHitsHFT[2]); }
  Int_t    numberOfSsdHits() const      { return (Int_t)(mNHitsHFT[3]); }
        
  Int_t    nVpdHitsEast() const     { return (Int_t)mNVpdHitsEast; }
  Int_t    nVpdHitsWest() const     { return (Int_t)mNVpdHitsWest; } 
  Int_t    nT0() const              { return (Int_t)mNT0; }
  Float_t  vzVpd() const            { return (Float_t)mVzVpd/100.; }
  
  Float_t  ZDCx() const             { return (Float_t)mZDCx; }
  Float_t  BBCx() const             { return (Float_t)mBBCx; }
  Float_t  Vpd(int i) const         { return (Float_t)mVpd[i]; }
  Float_t  ZdcSumAdcEast() const    { return (Float_t)mZdcSumAdcEast; }
  Float_t  ZdcSumAdcWest() const    { return (Float_t)mZdcSumAdcWest; }
  Float_t  ZdcSmdEastHorizontal(int i) const { return (Float_t)mZdcSmdEastHorizontal[i]; }
  Float_t  ZdcSmdEastVertical(int i) const   { return (Float_t)mZdcSmdEastVertical[i]; }
  Float_t  ZdcSmdWestHorizontal(int i) const { return (Float_t)mZdcSmdWestHorizontal[i]; }
  Float_t  ZdcSmdWestVertical(int i) const   { return (Float_t)mZdcSmdWestVertical[i]; }

  Float_t backgroundRate() const             { return mBackgroundRate; }
  Float_t bbcBlueBackgroundRate() const      { return mBbcBlueBackgroundRate; }
  Float_t bbcYellowBackgroundRate() const    { return mBbcYellowBackgroundRate; }
  Float_t bbcEastRate() const { return mBbcEastRate; }
  Float_t bbcWestRate() const { return mBbcWestRate; }  
  Float_t zdcEastRate() const { return mZdcEastRate; }
  Float_t zdcWestRate() const { return mZdcWestRate; }
  
  Float_t spaceCharge() const { return mSpaceCharge; }

  UShort_t btofTrayMultiplicity() const { return mbTofTrayMultiplicity ; }
  UShort_t numberOfGlobalTracks() const { return mNumberOfGlobalTracks ; }

  Float_t ranking() const { return mRanking ; }
  UShort_t nBEMCMatch() const { return mNBEMCMatch ; }
  UShort_t nBTOFMatch() const { return mNBTOFMatch ; }
  
  UShort_t bbcAdcEast(const Int_t i) { return mBbcAdcEast[i]; }
  UShort_t bbcAdcWest(const Int_t i) { return mBbcAdcWest[i]; }

  Int_t   ht_th(const Int_t i) { return mHT_Th[i]; }
  Int_t   jp_th(const Int_t i) { return mJP_Th[i]; }
  
#if 0
  TVector2 Q() const         { return TVector2(mQx,mQy); }
  TVector2 Q_ran_1() const   { return TVector2(mQx_ran_1,mQy_ran_1); }
  TVector2 Q_ran_2() const   { return TVector2(mQx_ran_2,mQy_ran_2); }
  TVector2 Q_chg_pos() const { return TVector2(mQx_chg_pos,mQy_chg_pos); }
  TVector2 Q_chg_neg() const { return TVector2(mQx_chg_neg,mQy_chg_neg); }
  TVector2 Q_eta_pos() const { return TVector2(mQx_eta_pos,mQy_eta_pos); }
  TVector2 Q_eta_neg() const { return TVector2(mQx_eta_neg,mQy_eta_neg); }
#endif
  TVector2 Q() const         { return TVector2(-9999.,-9999.); }
  TVector2 Q_ran_1() const   { return TVector2(-9999.,-9999.); }
  TVector2 Q_ran_2() const   { return TVector2(-9999.,-9999.); }
  TVector2 Q_chg_pos() const { return TVector2(-9999.,-9999.); }
  TVector2 Q_chg_neg() const { return TVector2(-9999.,-9999.); }
  TVector2 Q_eta_pos() const { return TVector2(-9999.,-9999.); }
  TVector2 Q_eta_neg() const { return TVector2(-9999.,-9999.); }

// other user's functions
  int      year() const;
  int      day() const;
  float    energy() const;
  bool     isMinBias() const;
  bool     isMBSlow() const;
  bool     isCentral() const;
  bool     isHT() const;    
  bool     isHT11() const; 
  bool     isHT15() const; 
  bool     isHT18() const;
  bool     isMtdTrig() const;
  bool     isDiMuon() const;
  bool     isDiMuonHFT() const;
  bool     isSingleMuon() const;
  bool     isEMuon() const;

// set functions for trigger thresholds
  void     setHT_Th(const Int_t i, const Int_t th) { mHT_Th[i] = (UChar_t)th; }
  void     setJP_Th(const Int_t i, const Int_t th) { mJP_Th[i] = (UChar_t)th; }  
      
protected: //these are written out
  Int_t          mRunId;           // run number
  Int_t          mEventId;         // event number
  UShort_t       mFillId;          // fill number
  Float_t        mBField;          // B field in kilogauss
  StThreeVectorF mPrimaryVertex;   // primary Vertex (1st)
  StThreeVectorF mSecondVertex;    // second Vertex position (for study)
  StThreeVectorF mPrimaryVertexError; // vertex error
  UInt_t         mTriggerWord;     // self-defined trigger word - see code for details
  UInt_t         mTriggerWordMtd;  // Added in Run14, st_mtd trigger
  UShort_t       mRefMultFtpcEast; // FTPC refMult east
  UShort_t       mRefMultFtpcWest; // FTPC refMult west
  UShort_t       mRefMultNeg;      // TPC refMult neg
  UShort_t       mRefMultPos;      // TPC refMult pos
  UShort_t       mRefMult2NegEast; // TPC refMult2 neg (-1<eta<-0.5)
  UShort_t       mRefMult2PosEast; // TPC refMult2 pos (-1<eta<-0.5)
  UShort_t       mRefMult2NegWest; // TPC refMult2 neg (0.5<eta<1.0)
  UShort_t       mRefMult2PosWest; // TPC refMult2 pos (0.5<eta<1.0)
  UShort_t       mRefMultHalfNegEast ;// TPC refMultHalf neg (eta<0)
  UShort_t       mRefMultHalfPosEast ;// TPC refMultHalf pos (eta<0)
  UShort_t       mRefMultHalfNegWest ;// TPC refMultHalf neg (eta>0)
  UShort_t       mRefMultHalfPosWest ;// TPC refMultHalf pos (eta>0)
  UShort_t       mGRefMult;
  UShort_t       mNHitsHFT[4];
  
  UChar_t        mNVpdHitsEast;    // Vpd Hits east;
  UChar_t        mNVpdHitsWest;    // vpd hits west;
  UShort_t       mNT0;             // number of T0 particles in BTOF self calibration
  Short_t        mVzVpd;           // VzVpd*100.

  UInt_t        mZDCx;           // zdcX
  UInt_t        mBBCx;
  Float_t mBackgroundRate;
  Float_t mBbcBlueBackgroundRate;
  Float_t mBbcYellowBackgroundRate;
  Float_t mBbcEastRate;
  Float_t mBbcWestRate;
  Float_t mZdcEastRate;
  Float_t mZdcWestRate;
  //Nov.10, 2008, Na
  UShort_t mVpd[64];
  UShort_t mZdcSumAdcEast;
  UShort_t mZdcSumAdcWest;
  UShort_t mZdcSmdEastHorizontal[8];
  UShort_t mZdcSmdEastVertical[8];
  UShort_t mZdcSmdWestHorizontal[8];
  UShort_t mZdcSmdWestVertical[8];
  Float_t mSpaceCharge;

  UShort_t mbTofTrayMultiplicity ; // BTOF tray multiplicity
  UShort_t mNumberOfGlobalTracks ; // # of global tracks

  // From StMuPrimaryVertex
  Float_t mRanking ;
  UShort_t mNBEMCMatch ;
  UShort_t mNBTOFMatch ;

  // BBC ADC for q-vectors (Hiroshi)
  UShort_t mBbcAdcEast[24] ; /// BBC East ADC: 0-23
  UShort_t mBbcAdcWest[24] ; /// BBC West ADC: 24-47

  // Online HT/JP thresholds
  UChar_t mHT_Th[4];
  UChar_t mJP_Th[4];
    
#if 0
  Float_t mQx;
  Float_t mQy;
  
  Float_t mQx_ran_1;
  Float_t mQy_ran_1;
  Float_t mQx_ran_2;
  Float_t mQy_ran_2;
  
  Float_t mQx_chg_pos;
  Float_t mQy_chg_pos;
  Float_t mQx_chg_neg;
  Float_t mQy_chg_neg;
  
  Float_t mQx_eta_pos;
  Float_t mQy_eta_pos;
  Float_t mQx_eta_neg;
  Float_t mQy_eta_neg;
#endif

  friend class StPicoDst;
  friend class StPicoDstMaker;
    
  ClassDef(StPicoEvent,1)
};

#endif
