/**
 * \class StPicoEvent
 * \brief Stores global information about the event
 *
 * The StPicoEvent class keeps variables that characterize event.
 * Most of the members are copied from StMuEvent.
 */

#ifndef StPicoEvent_h
#define StPicoEvent_h

// C++ headers
#include <vector>

// ROOT headers
#include "TObject.h"
#include "TVector3.h"

//_________________
class StPicoEvent : public TObject {

 public:
  
  /// Default constructor
  StPicoEvent();
  /// Copy constructor
  StPicoEvent(const StPicoEvent &event);
  /// Destructor
  virtual ~StPicoEvent();
  /// Print some event information
  virtual void Print(const Char_t *option = "") const;

  //
  // Getters
  //

  /// Return run ID
  Int_t    runId() const;
  /// Return event ID
  Int_t    eventId() const;
  /// Return fill ID
  Int_t    fillId() const;
  /// Return magnetic field
  Float_t  bField() const;
  /// Return time stamp
  Int_t    time() const;

  /// Return primary vertex position
  TVector3 primaryVertex() const;
  /// Return primary vertex position error
  TVector3 primaryVertexError() const;
  /// Return primary vertex ranking
  Float_t  ranking() const;
  /// Return number of tracks that matched BEMC
  UShort_t nBEMCMatch() const;
  /// Return number of tracks that matched TOF
  UShort_t nBTOFMatch() const;

  /// Return trigger list of the current event
  std::vector<unsigned int> triggerIds() const;
  /// Check if the trigger is in the list of triggers
  /// that were fired in the current event
  bool isTrigger(unsigned int) const;

  /// Return RefMult estimated via positive tracks (-0.5<eta<0.5)
  Int_t    refMultPos() const;
  /// Return RefMult estimated via negative tracks (-0.5<eta<0.5)
  Int_t    refMultNeg() const;
  /// Return RefMult estimated via positive tracks in the east FTPC
  Int_t    refMultFtpcEast() const;
  /// Return RefMult estimated via positive tracks in the west FTPC
  Int_t    refMultFtpcWest() const;
  /// Return RefMult (-0.5<eta<0.5)
  Int_t    refMult() const;
  /// Return RefMult measured in FTPC
  Int_t    refMultFtpc() const;
  /// Return RefMult estimated via positive tracks (-1<eta<-0.5)
  Int_t    refMult2PosEast() const;
  /// Return RefMult estimated via negative tracks (-1<eta<-0.5)
  Int_t    refMult2NegEast() const;
  /// Return RefMult estimated via positive tracks (0.5<eta<1)
  Int_t    refMult2PosWest() const;
  /// Return RefMult estimated via negative tracks (0.5<eta<1)
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
  /// Return RefMult2 value estimated by east TPC (-1<eta<0.5)
  Int_t    refMult2East() const;
  /// Return RefMult2 value estimated by west TPC (0.5<eta<1)
  Int_t    refMult2West() const;
  /// Return RefMult measured in 0.5<|RefMult|<1
  Int_t    refMult2() const;
  /// Return RefMult measured in the east TPC
  Int_t    refMultHalfEast() const;
  /// Return RefMult measured in the west TPC
  Int_t    refMultHalfWest() const;
  Int_t    refMult3East() const;
  Int_t    refMult3West() const;
  Int_t    refMult3() const;
  Int_t    refMult4East() const;
  Int_t    refMult4West() const;
  Int_t    refMult4() const;

  /// Return gRefMult (RefMult by global tracks in |gRefMult|<0.5)
  Int_t    grefMult() const;
  /// Return total number of global tracks that were reconstructed in the event
  UShort_t numberOfGlobalTracks() const;
  /// Return number of hits in TOF trays
  UShort_t btofTrayMultiplicity() const;
  /// Return number of hits in the inner PXL detector
  Int_t    numberOfPxlInnerHits() const;
  /// Return number of hits in the outer PXL detector
  Int_t    numberOfPxlOuterHits() const;
  /// Return number of hits in the IST
  Int_t    numberOfIstHits() const;
  /// Return number of hits in SSD
  Int_t    numberOfSsdHits() const;

  /// Return number of hits in the east VPD
  Int_t    nVpdHitsEast() const;
  /// Return number of hits in the west VPD
  Int_t    nVpdHitsWest() const;
  /// Return number of TOF tracks used for T0 calibration
  Int_t    nTofT0() const;
  /// Return z position of the primary vertex estimated by VPD
  Float_t  vzVpd() const;

  /// Return ZDC coincidence rate
  Float_t  ZDCx() const;
  /// Return BBC coincidence rate
  Float_t  BBCx() const;
  /// Return background rate
  Float_t  backgroundRate() const;
  /// Return "blue"-beam background rate
  Float_t  bbcBlueBackgroundRate() const;
  /// Return "yellow"-beam background rate
  Float_t  bbcYellowBackgroundRate() const;
  /// Return east BBC rate
  Float_t  bbcEastRate() const;
  /// Return west BBC rate
  Float_t  bbcWestRate() const;
  /// Return east ZDC rate
  Float_t  zdcEastRate() const;
  /// Return west ZDC rate
  Float_t  zdcWestRate() const;

  /// Return sum of east ADC from ZDC
  Float_t  ZdcSumAdcEast() const;
  /// Return sum of west ADC from ZDC
  Float_t  ZdcSumAdcWest() const;
  /// Return ADC of horizontal east ZDC
  Float_t  ZdcSmdEastHorizontal(int i) const;
  /// Return ADC of vertical east ZDC
  Float_t  ZdcSmdEastVertical(int i) const;
  /// Return ADC of horizontal west ZDC
  Float_t  ZdcSmdWestHorizontal(int i) const;
  /// Return ADC of vertical west ZDC
  Float_t  ZdcSmdWestVertical(int i) const;

  /// Return ADC of east BBC i-th channel
  UShort_t bbcAdcEast(const Int_t i) const;
  /// Return ADC of west BBC i-th channel
  UShort_t bbcAdcWest(const Int_t i) const;

  /// Return hith tower threshold
  Int_t    highTowerThreshold(const Int_t i) const;
  /// Return jet patch threshold
  Int_t    jetPatchThreshold(const Int_t i) const;

  /// Return year
  Int_t    year() const;
  /// Return day number
  Int_t    day() const;
  /// Return bunch crossing number
  Int_t    bunchId() const;

  //
  // Setters
  //

  /// Set run ID
  void setRunId(Int_t id);
  /// Set event ID
  void setEventId(Int_t id);
  /// Set fill ID
  void setFillId(Int_t id);
  /// Set fill ID
  void setFillId(Float_t id);
  /// Set magnetic field
  void setBField(Double_t bField);
  /// Set magnetic field
  void setMagneticField(Double_t bField);
  /// Set time stamp
  void setTime(Int_t time);

  /// Set primary vertex position (x,y,z)
  void setPrimaryVertexPosition(Float_t x, Float_t y, Float_t z);
  /// Set primary vertex position (3-vector)
  void setPrimaryVertexPosition(TVector3 position);
  /// Set primary vertex position error (ex,ey,ez)
  void setPrimaryVertexPositionError(Float_t x, Float_t y, Float_t z);
  /// Set primary vertex position error (3-vector)
  void setPrimaryVertexPositionError(TVector3 position);
  /// Set primary vertex ranking
  void setPrimaryVertexRanking(Float_t ranking);
  /// Set number of BEMC-matched tracks
  void setNumberOfBEMCMatch(Int_t n);
  //// Set number of TOF-matched tracks
  void setNumberOfBTOFMatch(Int_t n);

  /// Set trigger id
  void setTriggerId(UInt_t id);
  /// Set trigger id (pass STL vector with trigger IDs)
  void setTriggerIds(std::vector<unsigned int> ids);

  /// Set east RefMult (-1<eta<0.5) estimated by FTPC
  void setRefMultFtpcEast(UShort_t mult);
  /// Set west RefMult (-1<eta<0.5) estimated by FTPC
  void setRefMultFtpcWest(UShort_t mult);
  /// Set RefMult negative (|eta|<0.5)  
  void setRefMultNeg(UShort_t mult);
  /// Set RefMult positive (|eta|<0.5)
  void setRefMultPos(UShort_t mult);
  /// Set negative RefMult2 east ( -1<eta<-0.5 )
  void setRefMult2NegEast(UShort_t mult);
  /// Set positive RefMult2 east ( -1<eta<-0.5 )
  void setRefMult2PosEast(UShort_t mult);
  /// Set negative RefMult2 west ( 0.5<eta<1 )
  void setRefMult2NegWest(UShort_t mult);
  /// Set positive RefMult2 west ( 0.5<eta<1 )
  void setRefMult2PosWest(UShort_t mult);
  void setRefMult3NegEast(UShort_t mult);
  void setRefMult3PosEast(UShort_t mult);
  void setRefMult3NegWest(UShort_t mult);
  void setRefMult3PosWest(UShort_t mult);
  void setRefMult4NegEast(UShort_t mult);
  void setRefMult4PosEast(UShort_t mult);
  void setRefMult4NegWest(UShort_t mult);
  void setRefMult4PosWest(UShort_t mult);
  /// TPC refMultHalf neg (eta<0)
  void setRefMultHalfNegEast(UShort_t mult);
  /// TPC refMultHalf pos (eta<0)
  void setRefMultHalfPosEast(UShort_t mult);
  /// TPC refMultHalf neg (eta>0)
  void setRefMultHalfNegWest(UShort_t mult);
  /// TPC refMultHalf pos (eta>0)
  void setRefMultHalfPosWest(UShort_t mult);

  /// Set RefMult estimated by global tracks
  void setGRefMult(UShort_t mult);
  /// Set number of global tracks reconstructed in the event
  void setNumberOfGlobalTracks(UShort_t mult);
  /// Set total number of hits in TOF trays
  void setbTofTrayMultiplicity(UShort_t mult);
  /// Set number of hits in i-th HFT layers (PXL, PXL, IST, SSD)
  void setNHitsHFT(Int_t layer, UShort_t word);

  /// Set number of hits in the east VPD
  void setNVpdHitsEast(UShort_t nHits);
  /// Set number of hits in the west VPD
  void setNVpdHitsWest(UShort_t nHits);
  /// Set number of T0 particles in BTOF self calibration
  void setNTofT0(Int_t t0);
  /// Set Vz of the primary vertex reconstructed by VPD
  void setVzVpd(Float_t vpdVz);

  /// Set ZDC coincidence rate
  void setZDCx(Float_t zdcCoinRate);
  /// Set BBC coincidence rate
  void setBBCx(Float_t bbcCoinRate);
  /// Set background rate
  void setBackgroundRate(Float_t bckgRate);
  /// Set "blue"-beam background rate
  void setBbcBlueBackgroundRate(Float_t bbcBlueBckgRate);
  /// Set "yellow"-beam background rate
  void setBbcYellowBackgroundRate(Float_t bbcYellowBckgRate);
  /// Set east BBC rate
  void setBbcEastRate(Float_t bbcEastRate);
  /// Set west BBC rate
  void setBbcWestRate(Float_t bbcWestRate);
  /// Set east ZDC rate
  void setZdcEastRate(Float_t zdcEastRate);
  /// Set west ZDC rate
  void setZdcWestRate(Float_t zdcWestRate);

  /// Set sum of east ZDC ADC 
  void setZdcSumAdcEast(Float_t zdcSumAdcEast);
  /// Set sum of west ZDC ADC 
  void setZdcSumAdcWest(Float_t zdcSumAdcWest);
  /// Set ZDC for the east horizontal ZDC i-th strip
  void setZdcSmdEastHorizontal(Int_t strip, Float_t zdcSmdEastHorizontal);
  /// Set ZDC for the east vertical ZDC i-th strip
  void setZdcSmdEastVertical(Int_t strip, Float_t zdcSmdEastVertical);
  /// Set ZDC for the west horizontal ZDC i-th strip
  void setZdcSmdWestHorizontal(Int_t strip, Float_t zdcSmdWestHorizontal);
  /// Set ZDC for the west vertical ZDC i-th strip
  void setZdcSmdWestVertical(Int_t strip, Float_t zdcSmdWestVertical);

  /// Set i-th PMT of east BBC
  void setBbcAdcEast(Int_t iPMT, Float_t bbcAdcEast);
  /// Set i-th PMT of east BBC
  void setBbcAdcWest(Int_t iPMT, Float_t bbcAdcWest);

  void setHighTowerThreshold(const Int_t i, const Int_t th);
  void setJetPatchThreshold(const Int_t i, const Int_t th);

  /// Set bunch crossing ID
  void setBunchId(Int_t id);

protected:

  /// Run number (or runId)
  Int_t    mRunId;
  /// Event ID
  Int_t    mEventId;
  /// Fill number
  UShort_t mFillId;
  /// Magnetic field strength
  Float_t  mBField;

  /// To set timestamp for St_db_Maker. This is what StMuDstMaker used to 
  /// GetEvtHddr()->SetGMTime(cast into unsigned int). Peifeng Liu
  Int_t    mTime;

  /// Primary vertex position X
  Float_t mPrimaryVertexX;
  /// Primary vertex position Y
  Float_t mPrimaryVertexY;
  /// Primary vertex position Z
  Float_t mPrimaryVertexZ;
  /// Primary vertex position error X
  Float_t mPrimaryVertexErrorX;
  /// Primary vertex position error Y
  Float_t mPrimaryVertexErrorY;
  /// Primary vertex position error Z
  Float_t mPrimaryVertexErrorZ;

  /// Primary vertex ranking
  Float_t  mRanking;
  /// Number of BEMC-matched tracks
  UShort_t mNBEMCMatch;
  /// Number of TOF-matched tracks
  UShort_t mNBTOFMatch;

  /// List of triggers that were fired in the current event
  std::vector<unsigned int> mTriggerIds;

  /// RefMult for east TPC
  UShort_t mRefMultFtpcEast;
  /// RefMult for west TPC
  UShort_t mRefMultFtpcWest;
  /// RefMult estimated via negative tracks (-0.5<eta<0.5)
  UShort_t mRefMultNeg;
  /// RefMult estimated via positive tracks (-0.5<eta<0.5)
  UShort_t mRefMultPos;
  /// TPC refMult2 neg (-1<eta<-0.5)
  UShort_t mRefMult2NegEast;
  /// TPC refMult2 pos (-1<eta<-0.5)
  UShort_t mRefMult2PosEast;
  /// TPC refMult2 neg (0.5<eta<1.0)
  UShort_t mRefMult2NegWest;
  /// TPC refMult2 pos (0.5<eta<1.0)
  UShort_t mRefMult2PosWest; 
  UShort_t mRefMult3NegEast;
  UShort_t mRefMult3PosEast;
  UShort_t mRefMult3NegWest;
  UShort_t mRefMult3PosWest;
  UShort_t mRefMult4NegEast;
  UShort_t mRefMult4PosEast;
  UShort_t mRefMult4NegWest;
  UShort_t mRefMult4PosWest;
  /// TPC refMultHalf neg (eta<0)
  UShort_t mRefMultHalfNegEast;
  /// TPC refMultHalf pos (eta<0)
  UShort_t mRefMultHalfPosEast;
  // TPC refMultHalf neg (eta>0)
  UShort_t mRefMultHalfNegWest;
  /// TPC refMultHalf pos (eta>0)
  UShort_t mRefMultHalfPosWest;

  /// RefMult estimated by global tracks
  UShort_t mGRefMult;
  /// Total number of global tracks reconstructed in the event
  UShort_t mNumberOfGlobalTracks ;
  /// Total hit multiplicity in TOF trays
  UShort_t mbTofTrayMultiplicity ;
  /// Number of hits int HFT (in each of 4 layers)
  UShort_t mNHitsHFT[4];

  /// Number of hits in east VPD
  UChar_t  mNVpdHitsEast;
  /// Number of hits in west VPD
  UChar_t  mNVpdHitsWest;
  /// Number of T0 particles in BTOF self calibration
  UShort_t mNTofT0;
  /// Vz estimated via VPD
  Float_t  mVzVpd;

  /// ZDC coincidence rate
  UInt_t   mZDCx;
  /// BBC coincidence rate
  UInt_t   mBBCx;
  /// Background rate
  Float_t  mBackgroundRate;
  /// "Blue" beam rate measured in BBC
  Float_t  mBbcBlueBackgroundRate;
  /// "Yellow" beam rate measured in BBC
  Float_t  mBbcYellowBackgroundRate;
  /// East BBC rate
  Float_t  mBbcEastRate;
  /// West BBC rate
  Float_t  mBbcWestRate;
  /// East ZDC rate
  Float_t  mZdcEastRate;
  /// West ZDC rate
  Float_t  mZdcWestRate;

  /// Sum of ADC in east ZDC
  UShort_t mZdcSumAdcEast;
  /// Sum of ADC in west ZDC
  UShort_t mZdcSumAdcWest;
  /// ADC measured in 8 east horizontal ZDC strips
  UShort_t mZdcSmdEastHorizontal[8];
  /// ADC measured in 8 east vertical ZDC strips 
  UShort_t mZdcSmdEastVertical[8];
  /// ADC measured in 8 west horizontal ZDC strips
  UShort_t mZdcSmdWestHorizontal[8];
  /// ADC measured in 8 west vertical ZDC strips
  UShort_t mZdcSmdWestVertical[8];

  /// ADC measured in each of 24 tiles in east BBC
  UShort_t mBbcAdcEast[24];
  /// ADC measured in each of 24 tiles in west BBC
  UShort_t mBbcAdcWest[24];

  /// Online HT thresholds
  UChar_t  mHighTowerThreshold[4];
  /// Online JP thresholds BEMC only
  UChar_t  mJetPatchThreshold[4];

  /// Number of bunch crossing
  UChar_t  mBunchCrossId;

  ClassDef(StPicoEvent, 3)
};

//
// Getters
//
inline Int_t StPicoEvent::runId() const { return mRunId; }
inline Int_t StPicoEvent::eventId() const { return mEventId; }
inline Int_t StPicoEvent::fillId() const { return (Int_t)mFillId; }
inline Float_t StPicoEvent::bField() const { return mBField; }
inline Int_t StPicoEvent::time() const { return mTime; }

inline TVector3 StPicoEvent::primaryVertex() const {
  return TVector3( mPrimaryVertexX,mPrimaryVertexY,mPrimaryVertexZ);
}
inline TVector3 StPicoEvent::primaryVertexError() const {
  return TVector3(mPrimaryVertexErrorX,mPrimaryVertexErrorY,mPrimaryVertexErrorZ);
}

inline Float_t StPicoEvent::ranking() const { return mRanking; }
inline UShort_t StPicoEvent::nBEMCMatch() const { return mNBEMCMatch; }
inline UShort_t StPicoEvent::nBTOFMatch() const { return mNBTOFMatch; }

inline std::vector<unsigned int> StPicoEvent::triggerIds() const { return mTriggerIds; }

inline Int_t StPicoEvent::refMultPos() const { return (Int_t)mRefMultPos; }
inline Int_t StPicoEvent::refMultNeg() const { return (Int_t)mRefMultNeg; }
inline Int_t StPicoEvent::refMultFtpcEast() const { return (Int_t)mRefMultFtpcEast; }
inline Int_t StPicoEvent::refMultFtpcWest() const { return (Int_t)mRefMultFtpcWest; }
inline Int_t StPicoEvent::refMult() const { return (Int_t)(mRefMultPos + mRefMultNeg); }
inline Int_t StPicoEvent::refMultFtpc() const { return (Int_t)(mRefMultFtpcEast + mRefMultFtpcWest); }
inline Int_t StPicoEvent::refMult2PosEast() const { return (Int_t)mRefMult2PosEast; }
inline Int_t StPicoEvent::refMult2NegEast() const { return (Int_t)mRefMult2NegEast; }
inline Int_t StPicoEvent::refMult2PosWest() const { return (Int_t)mRefMult2PosWest; }
inline Int_t StPicoEvent::refMult2NegWest() const { return (Int_t)mRefMult2NegWest; }
inline Int_t StPicoEvent::refMult3PosEast() const { return (Int_t)mRefMult3PosEast; }
inline Int_t StPicoEvent::refMult3NegEast() const { return (Int_t)mRefMult3NegEast; }
inline Int_t StPicoEvent::refMult3PosWest() const { return (Int_t)mRefMult3PosWest; }
inline Int_t StPicoEvent::refMult3NegWest() const { return (Int_t)mRefMult3NegWest; }
inline Int_t StPicoEvent::refMult4PosEast() const { return (Int_t)mRefMult4PosEast; }
inline Int_t StPicoEvent::refMult4NegEast() const { return (Int_t)mRefMult4NegEast; }
inline Int_t StPicoEvent::refMult4PosWest() const { return (Int_t)mRefMult4PosWest; }
inline Int_t StPicoEvent::refMult4NegWest() const { return (Int_t)mRefMult4NegWest; }
inline Int_t StPicoEvent::refMultHalfPosEast() const { return (Int_t)mRefMultHalfPosEast; }
inline Int_t StPicoEvent::refMultHalfNegEast() const { return (Int_t)mRefMultHalfNegEast; }
inline Int_t StPicoEvent::refMultHalfPosWest() const { return (Int_t)mRefMultHalfPosWest; }
inline Int_t StPicoEvent::refMultHalfNegWest() const { return (Int_t)mRefMultHalfNegWest; }
inline Int_t StPicoEvent::refMult2East() const { return (Int_t)(mRefMult2PosEast + mRefMult2NegEast); }
inline Int_t StPicoEvent::refMult2West() const { return (Int_t)(mRefMult2PosWest + mRefMult2NegWest); }
inline Int_t StPicoEvent::refMult2() const { return (Int_t)(mRefMult2PosEast + mRefMult2NegEast + mRefMult2PosWest + mRefMult2NegWest); }
inline Int_t StPicoEvent::refMultHalfEast() const { return (Int_t)(mRefMultHalfPosEast + mRefMultHalfNegEast); }
inline Int_t StPicoEvent::refMultHalfWest() const { return (Int_t)(mRefMultHalfPosWest + mRefMultHalfNegWest); }
inline Int_t StPicoEvent::refMult3East() const { return (Int_t)(mRefMult3PosEast + mRefMult3NegEast); }
inline Int_t StPicoEvent::refMult3West() const { return (Int_t)(mRefMult3PosWest + mRefMult3NegWest); }
inline Int_t StPicoEvent::refMult3() const { return (Int_t)(mRefMult3PosEast + mRefMult3NegEast + mRefMult3PosWest + mRefMult3NegWest); }
inline Int_t StPicoEvent::refMult4East() const { return (Int_t)(mRefMult4PosEast + mRefMult4NegEast); }
inline Int_t StPicoEvent::refMult4West() const { return (Int_t)(mRefMult4PosWest + mRefMult4NegWest); }
inline Int_t StPicoEvent::refMult4() const { return (Int_t)(mRefMult4PosEast + mRefMult4NegEast + mRefMult4PosWest + mRefMult4NegWest); }

inline Int_t StPicoEvent::grefMult() const { return (Int_t)(mGRefMult); }
inline UShort_t StPicoEvent::numberOfGlobalTracks() const { return mNumberOfGlobalTracks; }
inline UShort_t StPicoEvent::btofTrayMultiplicity() const { return mbTofTrayMultiplicity; }
inline Int_t StPicoEvent::numberOfPxlInnerHits() const { return (Int_t)(mNHitsHFT[0]); }
inline Int_t StPicoEvent::numberOfPxlOuterHits() const { return (Int_t)(mNHitsHFT[1]); }
inline Int_t StPicoEvent::numberOfIstHits() const { return (Int_t)(mNHitsHFT[2]); }
inline Int_t StPicoEvent::numberOfSsdHits() const { return (Int_t)(mNHitsHFT[3]); }

inline Int_t StPicoEvent::nVpdHitsEast() const { return (Int_t)mNVpdHitsEast; }
inline Int_t StPicoEvent::nVpdHitsWest() const { return (Int_t)mNVpdHitsWest; }
inline Int_t StPicoEvent::nTofT0() const { return (Int_t)mNTofT0; }
inline Float_t StPicoEvent::vzVpd() const { return mVzVpd; }

inline Float_t StPicoEvent::ZDCx() const { return (Float_t)mZDCx; }
inline Float_t StPicoEvent::BBCx() const { return (Float_t)mBBCx; }
inline Float_t StPicoEvent::backgroundRate() const { return mBackgroundRate; }
inline Float_t StPicoEvent::bbcBlueBackgroundRate() const { return mBbcBlueBackgroundRate; }
inline Float_t StPicoEvent::bbcYellowBackgroundRate() const { return mBbcYellowBackgroundRate; }
inline Float_t StPicoEvent::bbcEastRate() const { return mBbcEastRate; }
inline Float_t StPicoEvent::bbcWestRate() const { return mBbcWestRate; }
inline Float_t StPicoEvent::zdcEastRate() const { return mZdcEastRate; }
inline Float_t StPicoEvent::zdcWestRate() const { return mZdcWestRate; }

inline Float_t StPicoEvent::ZdcSumAdcEast() const { return (Float_t)mZdcSumAdcEast; }
inline Float_t StPicoEvent::ZdcSumAdcWest() const { return (Float_t)mZdcSumAdcWest; }
inline Float_t StPicoEvent::ZdcSmdEastHorizontal(int i) const { return (Float_t)mZdcSmdEastHorizontal[i]; }
inline Float_t StPicoEvent::ZdcSmdEastVertical(int i) const { return (Float_t)mZdcSmdEastVertical[i]; }
inline Float_t StPicoEvent::ZdcSmdWestHorizontal(int i) const { return (Float_t)mZdcSmdWestHorizontal[i]; }
inline Float_t StPicoEvent::ZdcSmdWestVertical(int i) const { return (Float_t)mZdcSmdWestVertical[i]; }

inline UShort_t StPicoEvent::bbcAdcEast(const Int_t i) const { return mBbcAdcEast[i]; }
inline UShort_t StPicoEvent::bbcAdcWest(const Int_t i) const { return mBbcAdcWest[i]; }

inline Int_t StPicoEvent::highTowerThreshold(const Int_t i) const { return mHighTowerThreshold[i]; }

inline Int_t StPicoEvent::jetPatchThreshold(const Int_t i) const { return mJetPatchThreshold[i]; }

inline Int_t StPicoEvent::bunchId() const { return (Int_t)mBunchCrossId; }

//
// Setters
//
inline void StPicoEvent::setRunId(Int_t runId) { mRunId = runId; }
inline void StPicoEvent::setEventId(Int_t id) { mEventId = id; }
inline void StPicoEvent::setFillId(Int_t id) { mFillId = (UShort_t)id; }
inline void StPicoEvent::setFillId(Float_t id) { mFillId = (id > 0) ? (UShort_t)id : 0; }
inline void StPicoEvent::setBField(Double_t bField) { mBField = (Float_t)bField; }
inline void StPicoEvent::setMagneticField(Double_t bField) { mBField = (Float_t)bField; }
inline void StPicoEvent::setTime(Int_t time) { mTime = time; }

inline void StPicoEvent::setPrimaryVertexPosition(Float_t x, Float_t y, Float_t z) {
  mPrimaryVertexX = x; mPrimaryVertexY = y; mPrimaryVertexZ = z;
}
inline void StPicoEvent::setPrimaryVertexPosition(TVector3 vtxPos) {
  mPrimaryVertexX = vtxPos.X(); mPrimaryVertexY = vtxPos.Y(); mPrimaryVertexZ = vtxPos.Z();
}
inline void StPicoEvent::setPrimaryVertexPositionError(Float_t x, Float_t y, Float_t z) {
  mPrimaryVertexErrorX = x; mPrimaryVertexErrorY = y; mPrimaryVertexErrorZ = z;
}
inline void StPicoEvent::setPrimaryVertexPositionError(TVector3 vtxPosErr) {
  mPrimaryVertexErrorX = vtxPosErr.X(); mPrimaryVertexErrorY = vtxPosErr.Y(); mPrimaryVertexErrorZ = vtxPosErr.Z();
}

inline void StPicoEvent::setPrimaryVertexRanking(Float_t ranking) { mRanking = (Float_t)ranking; }
inline void StPicoEvent::setNumberOfBEMCMatch(Int_t n) { mNBEMCMatch = (UShort_t)n; }
inline void StPicoEvent::setNumberOfBTOFMatch(Int_t n) { mNBTOFMatch = (UShort_t)n; }

inline void StPicoEvent::setRefMultFtpcEast(UShort_t mult) { mRefMultFtpcEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMultFtpcWest(UShort_t mult) { mRefMultFtpcWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMultNeg(UShort_t mult) { mRefMultNeg = (UShort_t)mult; }
inline void StPicoEvent::setRefMultPos(UShort_t mult) { mRefMultPos = (UShort_t)mult; }
inline void StPicoEvent::setRefMult2NegEast(UShort_t mult) { mRefMult2NegEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMult2PosEast(UShort_t mult) { mRefMult2PosEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMult2NegWest(UShort_t mult) { mRefMult2NegWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMult2PosWest(UShort_t mult) { mRefMult2PosWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMult3NegEast(UShort_t mult) { mRefMult3NegEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMult3PosEast(UShort_t mult) { mRefMult3PosEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMult3NegWest(UShort_t mult) { mRefMult3NegWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMult3PosWest(UShort_t mult) { mRefMult3PosWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMult4NegEast(UShort_t mult) { mRefMult4NegEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMult4PosEast(UShort_t mult) { mRefMult4PosEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMult4NegWest(UShort_t mult) { mRefMult4NegWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMult4PosWest(UShort_t mult) { mRefMult4PosWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMultHalfNegEast(UShort_t mult) { mRefMultHalfNegEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMultHalfPosEast(UShort_t mult) { mRefMultHalfPosEast = (UShort_t)mult; }
inline void StPicoEvent::setRefMultHalfNegWest(UShort_t mult) { mRefMultHalfNegWest = (UShort_t)mult; }
inline void StPicoEvent::setRefMultHalfPosWest(UShort_t mult) { mRefMultHalfPosWest = (UShort_t)mult; }

inline void StPicoEvent::setGRefMult(UShort_t mult) { mGRefMult = (UShort_t)mult; }
inline void StPicoEvent::setNumberOfGlobalTracks(UShort_t mult) { mNumberOfGlobalTracks = (UShort_t)mult; }
inline void StPicoEvent::setbTofTrayMultiplicity(UShort_t mult) { mbTofTrayMultiplicity = (UShort_t)mult; }

inline void StPicoEvent::setNVpdHitsEast(UShort_t nHits) { mNVpdHitsEast = (UChar_t)nHits; }
inline void StPicoEvent::setNVpdHitsWest(UShort_t nHits) { mNVpdHitsWest = (UChar_t)nHits; };
inline void StPicoEvent::setNTofT0(Int_t t0) { mNTofT0 = (UShort_t)t0; } 
inline void StPicoEvent::setVzVpd(Float_t vpdVz) { mVzVpd = vpdVz; }

inline void StPicoEvent::setZDCx(Float_t zdcCoinRate) { mZDCx = (UInt_t)zdcCoinRate; }
inline void StPicoEvent::setBBCx(Float_t bbcCoinRate) { mBBCx = (UInt_t)bbcCoinRate; }
inline void StPicoEvent::setBackgroundRate(Float_t bckgRate) { mBackgroundRate = (Float_t)bckgRate; }
inline void StPicoEvent::setBbcBlueBackgroundRate(Float_t bbcBlueBckgRate) { mBbcBlueBackgroundRate = (Float_t)bbcBlueBckgRate; }
inline void StPicoEvent::setBbcYellowBackgroundRate(Float_t bbcYellowBckgRate) { mBbcYellowBackgroundRate = (Float_t)bbcYellowBckgRate; }
inline void StPicoEvent::setBbcEastRate(Float_t bbcEastRate) { mBbcEastRate = (Float_t)bbcEastRate; }
inline void StPicoEvent::setBbcWestRate(Float_t bbcWestRate) { mBbcWestRate = (Float_t)bbcWestRate; }
inline void StPicoEvent::setZdcEastRate(Float_t zdcEastRate) { mZdcEastRate = (Float_t)zdcEastRate; }
inline void StPicoEvent::setZdcWestRate(Float_t zdcWestRate) { mZdcWestRate = (Float_t)zdcWestRate; }

inline void StPicoEvent::setZdcSumAdcEast(Float_t zdcSumAdcEast) { mZdcSumAdcEast = (UShort_t)zdcSumAdcEast; }
inline void StPicoEvent::setZdcSumAdcWest(Float_t zdcSumAdcWest) { mZdcSumAdcWest = (UShort_t)zdcSumAdcWest; }

inline void StPicoEvent::setHighTowerThreshold(const Int_t i, const Int_t th) { mHighTowerThreshold[i] = (UChar_t)th; }
inline void StPicoEvent::setJetPatchThreshold(const Int_t i, const Int_t th) { mJetPatchThreshold[i] = (UChar_t)th; }

#endif
