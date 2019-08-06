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
  Int_t    runId() const               { return mRunId; }
  /// Return event ID
  Int_t    eventId() const             { return mEventId; }
  /// Return fill ID
  Int_t    fillId() const              { return (Int_t)mFillId; }
  /// Return magnetic field
  Float_t  bField() const              { return mBField; }
  /// Return time stamp
  Int_t    time() const                { return mTime; }

  /// Return primary vertex position
  TVector3 primaryVertex() const
  { return TVector3( mPrimaryVertexX,mPrimaryVertexY,mPrimaryVertexZ); }
  /// Return primary vertex position error
  TVector3 primaryVertexError() const
  { return TVector3(mPrimaryVertexErrorX,mPrimaryVertexErrorY,mPrimaryVertexErrorZ); }
  /// Return primary vertex ranking
  Float_t  ranking() const             { return mRanking; }
  /// Return number of tracks that matched BEMC
  UShort_t nBEMCMatch() const          { return mNBEMCMatch; }
  /// Return number of tracks that matched TOF
  UShort_t nBTOFMatch() const          { return mNBTOFMatch; }

  /// Return trigger list of the current event
  std::vector<unsigned int> triggerIds() const { return mTriggerIds; }
  /// Check if the trigger is in the list of triggers
  /// that were fired in the current event
  bool isTrigger(unsigned int) const;

  /// Return RefMult estimated via positive tracks (-0.5<eta<0.5)
  Int_t    refMultPos() const          { return (Int_t)mRefMultPos; }
  /// Return RefMult estimated via negative tracks (-0.5<eta<0.5)
  Int_t    refMultNeg() const          { return (Int_t)mRefMultNeg; }
  /// Return RefMult estimated via positive tracks in the east FTPC
  Int_t    refMultFtpcEast() const     { return (Int_t)mRefMultFtpcEast; }
  /// Return RefMult estimated via positive tracks in the west FTPC
  Int_t    refMultFtpcWest() const     { return (Int_t)mRefMultFtpcWest; }
  /// Return RefMult (-0.5<eta<0.5)
  Int_t    refMult() const             { return (Int_t)(mRefMultPos + mRefMultNeg); }
  /// Return RefMult measured in FTPC
  Int_t    refMultFtpc() const         { return (Int_t)(mRefMultFtpcEast + mRefMultFtpcWest); }
  /// Return RefMult estimated via positive tracks (-1<eta<-0.5)
  Int_t    refMult2PosEast() const     { return (Int_t)mRefMult2PosEast; }
  /// Return RefMult estimated via negative tracks (-1<eta<-0.5)
  Int_t    refMult2NegEast() const     { return (Int_t)mRefMult2NegEast; }
  /// Return RefMult estimated via positive tracks (0.5<eta<1)
  Int_t    refMult2PosWest() const     { return (Int_t)mRefMult2PosWest; }
  /// Return RefMult estimated via negative tracks (0.5<eta<1)
  Int_t    refMult2NegWest() const     { return (Int_t)mRefMult2NegWest; }
  Int_t    refMult3PosEast() const     { return (Int_t)mRefMult3PosEast; }
  Int_t    refMult3NegEast() const     { return (Int_t)mRefMult3NegEast; }
  Int_t    refMult3PosWest() const     { return (Int_t)mRefMult3PosWest; }
  Int_t    refMult3NegWest() const     { return (Int_t)mRefMult3NegWest; }
  Int_t    refMult4PosEast() const     { return (Int_t)mRefMult4PosEast; }
  Int_t    refMult4NegEast() const     { return (Int_t)mRefMult4NegEast; }
  Int_t    refMult4PosWest() const     { return (Int_t)mRefMult4PosWest; }
  Int_t    refMult4NegWest() const     { return (Int_t)mRefMult4NegWest; }
  Int_t    refMultHalfPosEast() const  { return (Int_t)mRefMultHalfPosEast; }
  Int_t    refMultHalfNegEast() const  { return (Int_t)mRefMultHalfNegEast; }
  Int_t    refMultHalfPosWest() const  { return (Int_t)mRefMultHalfPosWest; }
  Int_t    refMultHalfNegWest() const  { return (Int_t)mRefMultHalfNegWest; }
  /// Return RefMult2 value estimated by east TPC (-1<eta<0.5)
  Int_t    refMult2East() const        { return (Int_t)(mRefMult2PosEast + mRefMult2NegEast); }
  /// Return RefMult2 value estimated by west TPC (0.5<eta<1)
  Int_t    refMult2West() const        { return (Int_t)(mRefMult2PosWest + mRefMult2NegWest); }
  /// Return RefMult measured in 0.5<|RefMult|<1
  Int_t    refMult2() const
  { return (Int_t)(mRefMult2PosEast + mRefMult2NegEast + mRefMult2PosWest + mRefMult2NegWest); }
  /// Return RefMult measured in the east TPC
  Int_t    refMultHalfEast() const     { return (Int_t)(mRefMultHalfPosEast + mRefMultHalfNegEast); }
  /// Return RefMult measured in the west TPC
  Int_t    refMultHalfWest() const     { return (Int_t)(mRefMultHalfPosWest + mRefMultHalfNegWest); }
  Int_t    refMult3East() const        { return (Int_t)(mRefMult3PosEast + mRefMult3NegEast); }
  Int_t    refMult3West() const        { return (Int_t)(mRefMult3PosWest + mRefMult3NegWest); }
  Int_t    refMult3() const
  { return (Int_t)(mRefMult3PosEast + mRefMult3NegEast + mRefMult3PosWest + mRefMult3NegWest); }
  Int_t    refMult4East() const        { return (Int_t)(mRefMult4PosEast + mRefMult4NegEast); }
  Int_t    refMult4West() const        { return (Int_t)(mRefMult4PosWest + mRefMult4NegWest); }
  Int_t    refMult4() const
  { return (Int_t)(mRefMult4PosEast + mRefMult4NegEast + mRefMult4PosWest + mRefMult4NegWest); }

  /// Return gRefMult (RefMult by global tracks in |gRefMult|<0.5)
  Int_t    grefMult() const              { return (Int_t)(mGRefMult); }
  /// Return total number of global tracks that were reconstructed in the event
  UShort_t numberOfGlobalTracks() const  { return mNumberOfGlobalTracks; }
  /// Return number of hits in TOF trays
  UShort_t btofTrayMultiplicity() const  { return mbTofTrayMultiplicity; }
  /// Return number of hits in the inner PXL detector
  Int_t    numberOfPxlInnerHits() const  { return (Int_t)(mNHitsHFT[0]); }
  /// Return number of hits in the outer PXL detector
  Int_t    numberOfPxlOuterHits() const  { return (Int_t)(mNHitsHFT[1]); }
  /// Return number of hits in the IST
  Int_t    numberOfIstHits() const       { return (Int_t)(mNHitsHFT[2]); }
  /// Return number of hits in SSD
  Int_t    numberOfSsdHits() const       { return (Int_t)(mNHitsHFT[3]); }
  /// Return number of hits in ETOF modules
  UShort_t etofHitMultiplicity() const   { return mETofHitMultiplicity; }
  /// Return number of digis in ETOF modules
  UShort_t etofDigiMultiplicity() const  { return mETofDigiMultiplicity; }

  /// Return number of hits in the east VPD
  Int_t    nVpdHitsEast() const          { return (Int_t)mNVpdHitsEast; }
  /// Return number of hits in the west VPD
  Int_t    nVpdHitsWest() const          { return (Int_t)mNVpdHitsWest; }
  /// Return number of TOF tracks used for T0 calibration
  Int_t    nTofT0() const                { return (Int_t)mNTofT0; }
  /// Return z position of the primary vertex estimated by VPD
  Float_t  vzVpd() const                 { return mVzVpd; }

  /// Return ZDC coincidence rate
  Float_t  ZDCx() const                  { return (Float_t)mZDCx; }
  /// Return BBC coincidence rate
  Float_t  BBCx() const                  { return (Float_t)mBBCx; }
  /// Return background rate
  Float_t  backgroundRate() const        { return mBackgroundRate; }
  /// Return "blue"-beam background rate
  Float_t  bbcBlueBackgroundRate() const { return mBbcBlueBackgroundRate; }
  /// Return "yellow"-beam background rate
  Float_t  bbcYellowBackgroundRate() const { return mBbcYellowBackgroundRate; }
  /// Return east BBC rate
  Float_t  bbcEastRate() const           { return mBbcEastRate; }
  /// Return west BBC rate
  Float_t  bbcWestRate() const           { return mBbcWestRate; }
  /// Return east ZDC rate
  Float_t  zdcEastRate() const           { return mZdcEastRate; }
  /// Return west ZDC rate
  Float_t  zdcWestRate() const           { return mZdcWestRate; }

  /// Return sum of east ADC from ZDC
  Float_t  ZdcSumAdcEast() const         { return (Float_t)mZdcSumAdcEast; }
  /// Return sum of west ADC from ZDC
  Float_t  ZdcSumAdcWest() const         { return (Float_t)mZdcSumAdcWest; }
  /// Return ADC of horizontal east ZDC
  Float_t  ZdcSmdEastHorizontal(int i) const { return (Float_t)mZdcSmdEastHorizontal[i]; }
  /// Return ADC of vertical east ZDC
  Float_t  ZdcSmdEastVertical(int i) const   { return (Float_t)mZdcSmdEastVertical[i]; }
  /// Return ADC of horizontal west ZDC
  Float_t  ZdcSmdWestHorizontal(int i) const { return (Float_t)mZdcSmdWestHorizontal[i]; }
  /// Return ADC of vertical west ZDC
  Float_t  ZdcSmdWestVertical(int i) const   { return (Float_t)mZdcSmdWestVertical[i]; }

  /// Return ADC of east BBC i-th channel
  UShort_t bbcAdcEast(const Int_t i) const   { return mBbcAdcEast[i]; }
  /// Return ADC of west BBC i-th channel
  UShort_t bbcAdcWest(const Int_t i) const   { return mBbcAdcWest[i]; }

  /// Return hith tower threshold
  Int_t    highTowerThreshold(const Int_t i) const { return mHighTowerThreshold[i]; }
  /// Return jet patch threshold
  Int_t    jetPatchThreshold(const Int_t i) const  { return mJetPatchThreshold[i]; }

  /// Return year
  Int_t    year() const;
  /// Return day number
  Int_t    day() const;
  /// Return bunch crossing number
  Int_t    bunchId() const                    { return (Int_t)mBunchCrossId; }

  //
  // Setters
  //

  /// Set run ID
  void setRunId(Int_t id)                     { mRunId = id; }
  /// Set event ID
  void setEventId(Int_t id)                   { mEventId = id; }
  /// Set fill ID
  void setFillId(Int_t id)                    { mFillId = (UShort_t)id; }
  /// Set fill ID
  void setFillId(Float_t id)                  { mFillId = (id > 0) ? (UShort_t)id : 0; }
  /// Set magnetic field
  void setBField(Double_t bField)             { mBField = (Float_t)bField; }
  /// Set magnetic field
  void setMagneticField(Double_t bField)      { mBField = (Float_t)bField; }
  /// Set time stamp
  void setTime(Int_t time)                    { mTime = time; }

  /// Set primary vertex position (x,y,z)
  void setPrimaryVertexPosition(Float_t x, Float_t y, Float_t z)
  { mPrimaryVertexX = x; mPrimaryVertexY = y; mPrimaryVertexZ = z; }
  /// Set primary vertex position (3-vector)
  void setPrimaryVertexPosition(TVector3 vtxPos)
  { mPrimaryVertexX = vtxPos.X(); mPrimaryVertexY = vtxPos.Y(); mPrimaryVertexZ = vtxPos.Z(); }
  /// Set primary vertex position error (ex,ey,ez)
  void setPrimaryVertexPositionError(Float_t x, Float_t y, Float_t z)
  { mPrimaryVertexErrorX = x; mPrimaryVertexErrorY = y; mPrimaryVertexErrorZ = z; }
  /// Set primary vertex position error (3-vector)
  void setPrimaryVertexPositionError(TVector3 vtxPosErr)
  { mPrimaryVertexErrorX=vtxPosErr.X(); mPrimaryVertexErrorY=vtxPosErr.Y(); mPrimaryVertexErrorZ=vtxPosErr.Z(); }
  /// Set primary vertex ranking
  void setPrimaryVertexRanking(Float_t ranking) { mRanking = (Float_t)ranking; }
  /// Set number of BEMC-matched tracks
  void setNumberOfBEMCMatch(Int_t n)            { mNBEMCMatch = (UShort_t)n; }
  //// Set number of TOF-matched tracks
  void setNumberOfBTOFMatch(Int_t n)            { mNBTOFMatch = (UShort_t)n; }
  /// Set total number of hits in ETOF modules
  void setETofHitMultiplicity(UShort_t mult)    { mETofHitMultiplicity = (UShort_t)mult; }
  /// Set total number of digis in ETOF modules
  void setETofDigiMultiplicity(UShort_t mult)   { mETofDigiMultiplicity = (UShort_t)mult; }

  /// Set trigger id
  void setTriggerId(UInt_t id);
  /// Set trigger id (pass STL vector with trigger IDs)
  void setTriggerIds(std::vector<unsigned int> ids);

  /// Set east RefMult (-1<eta<0.5) estimated by FTPC
  void setRefMultFtpcEast(UShort_t mult)        { mRefMultFtpcEast = (UShort_t)mult; }
  /// Set west RefMult (-1<eta<0.5) estimated by FTPC
  void setRefMultFtpcWest(UShort_t mult)        { mRefMultFtpcWest = (UShort_t)mult; }
  /// Set RefMult negative (|eta|<0.5)  
  void setRefMultNeg(UShort_t mult)             { mRefMultNeg = (UShort_t)mult; }
  /// Set RefMult positive (|eta|<0.5)
  void setRefMultPos(UShort_t mult)             { mRefMultPos = (UShort_t)mult; }
  /// Set negative RefMult2 east ( -1<eta<-0.5 )
  void setRefMult2NegEast(UShort_t mult)        { mRefMult2NegEast = (UShort_t)mult; }
  /// Set positive RefMult2 east ( -1<eta<-0.5 )
  void setRefMult2PosEast(UShort_t mult)        { mRefMult2PosEast = (UShort_t)mult; }
  /// Set negative RefMult2 west ( 0.5<eta<1 )
  void setRefMult2NegWest(UShort_t mult)        { mRefMult2NegWest = (UShort_t)mult; }
  /// Set positive RefMult2 west ( 0.5<eta<1 )
  void setRefMult2PosWest(UShort_t mult)        { mRefMult2PosWest = (UShort_t)mult; }
  void setRefMult3NegEast(UShort_t mult)        { mRefMult3NegEast = (UShort_t)mult; }
  void setRefMult3PosEast(UShort_t mult)        { mRefMult3PosEast = (UShort_t)mult; }
  void setRefMult3NegWest(UShort_t mult)        { mRefMult3NegWest = (UShort_t)mult; }
  void setRefMult3PosWest(UShort_t mult)        { mRefMult3PosWest = (UShort_t)mult; }
  void setRefMult4NegEast(UShort_t mult)        { mRefMult4NegEast = (UShort_t)mult; }
  void setRefMult4PosEast(UShort_t mult)        { mRefMult4PosEast = (UShort_t)mult; }
  void setRefMult4NegWest(UShort_t mult)        { mRefMult4NegWest = (UShort_t)mult; }
  void setRefMult4PosWest(UShort_t mult)        { mRefMult4PosWest = (UShort_t)mult; }
  /// TPC refMultHalf neg (eta<0)
  void setRefMultHalfNegEast(UShort_t mult)     { mRefMultHalfNegEast = (UShort_t)mult; }
  /// TPC refMultHalf pos (eta<0)
  void setRefMultHalfPosEast(UShort_t mult)     { mRefMultHalfPosEast = (UShort_t)mult; }
  /// TPC refMultHalf neg (eta>0)
  void setRefMultHalfNegWest(UShort_t mult)     { mRefMultHalfNegWest = (UShort_t)mult; }
  /// TPC refMultHalf pos (eta>0)
  void setRefMultHalfPosWest(UShort_t mult)     { mRefMultHalfPosWest = (UShort_t)mult; }

  /// Set RefMult estimated by global tracks
  void setGRefMult(UShort_t mult)               { mGRefMult = (UShort_t)mult; }
  /// Set number of global tracks reconstructed in the event
  void setNumberOfGlobalTracks(UShort_t mult)   { mNumberOfGlobalTracks = (UShort_t)mult; }
  /// Set total number of hits in TOF trays
  void setbTofTrayMultiplicity(UShort_t mult)   { mbTofTrayMultiplicity = (UShort_t)mult; }
  /// Set number of hits in i-th HFT layers (PXL, PXL, IST, SSD)
  void setNHitsHFT(Int_t layer, UShort_t word);

  /// Set number of hits in the east VPD
  void setNVpdHitsEast(UShort_t nHits)          { mNVpdHitsEast = (UChar_t)nHits; }
  /// Set number of hits in the west VPD
  void setNVpdHitsWest(UShort_t nHits)          { mNVpdHitsWest = (UChar_t)nHits; };
  /// Set number of T0 particles in BTOF self calibration
  void setNTofT0(Int_t t0)                      { mNTofT0 = (UShort_t)t0; } 
  /// Set Vz of the primary vertex reconstructed by VPD
  void setVzVpd(Float_t vpdVz)                  { mVzVpd = vpdVz; }

  /// Set ZDC coincidence rate
  void setZDCx(Float_t zdcCoinRate)             { mZDCx = (UInt_t)zdcCoinRate; }
  /// Set BBC coincidence rate
  void setBBCx(Float_t bbcCoinRate)             { mBBCx = (UInt_t)bbcCoinRate; }
  /// Set background rate
  void setBackgroundRate(Float_t bckgRate)      { mBackgroundRate = (Float_t)bckgRate; }
  /// Set "blue"-beam background rate
  void setBbcBlueBackgroundRate(Float_t bbcBlueBckgRate) { mBbcBlueBackgroundRate = (Float_t)bbcBlueBckgRate; }
  /// Set "yellow"-beam background rate
  void setBbcYellowBackgroundRate(Float_t bbcYellowBckgRate)
  { mBbcYellowBackgroundRate = (Float_t)bbcYellowBckgRate; }
  /// Set east BBC rate
  void setBbcEastRate(Float_t bbcEastRate)      { mBbcEastRate = (Float_t)bbcEastRate; }
  /// Set west BBC rate
  void setBbcWestRate(Float_t bbcWestRate)      { mBbcWestRate = (Float_t)bbcWestRate; }
  /// Set east ZDC rate
  void setZdcEastRate(Float_t zdcEastRate)      { mZdcEastRate = (Float_t)zdcEastRate; }
  /// Set west ZDC rate
  void setZdcWestRate(Float_t zdcWestRate)      { mZdcWestRate = (Float_t)zdcWestRate; }

  /// Set sum of east ZDC ADC 
  void setZdcSumAdcEast(Float_t zdcSumAdcEast)  { mZdcSumAdcEast = (UShort_t)zdcSumAdcEast; }
  /// Set sum of west ZDC ADC 
  void setZdcSumAdcWest(Float_t zdcSumAdcWest)  { mZdcSumAdcWest = (UShort_t)zdcSumAdcWest; }
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

  /// Set threshold for the high tower
  void setHighTowerThreshold(const Int_t i, const Int_t th) { mHighTowerThreshold[i] = (UChar_t)th; }
  /// Set threshold for jet patch
  void setJetPatchThreshold(const Int_t i, const Int_t th)  { mJetPatchThreshold[i] = (UChar_t)th; }

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

  /// Total hit multiplicity in ETOF modules
  UShort_t mETofHitMultiplicity ;
  /// Total digi multiplicity in ETOF modules
  UShort_t mETofDigiMultiplicity ;

  ClassDef(StPicoEvent, 4)
};

#endif
