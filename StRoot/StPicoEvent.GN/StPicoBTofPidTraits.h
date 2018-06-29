#ifndef StPicoBTofPidTraits_h
#define StPicoBTofPidTraits_h

/// C++ headers
#include <limits>

/// ROOT headers
#include "TObject.h"
#include "TVector3.h"

//_________________
class StPicoBTofPidTraits : public TObject {

 public:
  /// Default constructor
  StPicoBTofPidTraits();
  /// Copy constructor
  StPicoBTofPidTraits(const StPicoBTofPidTraits &traits);
  /// Destructor
  virtual ~StPicoBTofPidTraits();
  /// Print TOF PID traits information
  virtual void Print(const Char_t* option = "") const;

  /**
   * Getters
   */
  Int_t   trackIndex() const;
  Int_t   btofCellId() const;
  Int_t   btofMatchFlag() const;
  Float_t btof() const { return mBTof; }
  Float_t btofBeta() const { return mBTofBeta; }
  Float_t btofYLocal() const { return mBTofYLocal; }
  Float_t btofZLocal() const { return mBTofZLocal; }
  TVector3 btofHitPos() const { return TVector3(mBTofHitPosX, mBTofHitPosY, mBTofHitPosZ); }
  Float_t btofHitPosX() const { return mBTofHitPosX; }
  Float_t btofHitPosY() const { return mBTofHitPosY; }
  Float_t btofHitPosZ() const { return mBTofHitPosZ; }

  /**
   * Setters
   */
  void setTrackIndex(Int_t inx2PicoTrack);
  void setBTofCellId(Int_t tray, Int_t module, Int_t cell);
  void setBTofMatchFlag(UChar_t flag);
  void setTOF(Float_t tof);
  void setBeta(Float_t beta);
  void setHitPositionXYZ(Float_t x, Float_t y, Float_t z);
  void setHitPositionX(Float_t x);
  void setHitPositionY(Float_t y);
  void setHitPositionZ(Float_t z);
  void setYLocal(Float_t yLocal);
  void setZLocal(Float_t zLocal);

 private:

  Short_t  mTrackIndex;       // Index to the associated picoTrack in the event
  Short_t  mBTofCellId;       // (tray-1)*192+(module-1)*6+(cell-1): -1 - no match
  UChar_t  mBTofMatchFlag;    // 0 - no match, 1 - one-to-one, 2 - one-to-multiple
#if 0
  UShort_t mBTof;             // time-Of-Flight * 1000 in ns
  UShort_t mBTofBeta;         // beta * 20000
  Short_t  mBTofYLocal;       // ylocal * 1000
  Short_t  mBTofZLocal;       // zlocal * 1000
  Short_t  mBTofHitPosX;      // projected hit position X * 100
  Short_t  mBTofHitPosY;      // projected hit position Y * 100
  Short_t  mBTofHitPosZ;      // projected hit position Z * 100
#else 
  Float16_t mBTof;             //[0,65,16] time-Of-Flightin  ns
  Float16_t mBTofBeta;         //[0,1.6,16] beta
  Float16_t mBTofYLocal;       //[-5,5,16]  ylocal
  Float16_t mBTofZLocal;       //[-5,5,16] zlocal
  Float16_t mBTofHitPosX;      //[-30,30,16] projected hit position X
  Float16_t mBTofHitPosY;      //[-30,30,16] projected hit position Y
  Float16_t mBTofHitPosZ;      //[-30,30,16] projected hit position Z
#endif

  ClassDef(StPicoBTofPidTraits, 1);
};

/**
 * Getters
 */
inline Int_t   StPicoBTofPidTraits::trackIndex() const { return mTrackIndex; }
inline Int_t   StPicoBTofPidTraits::btofCellId() const { return (Int_t)mBTofCellId; }
inline Int_t   StPicoBTofPidTraits::btofMatchFlag() const { return (Int_t)mBTofMatchFlag; }
#if 0
inline Float_t StPicoBTofPidTraits::btof() const { return (Float_t)mBTof / 1000.; }
inline Float_t StPicoBTofPidTraits::btofBeta() const { return (Float_t)mBTofBeta / 20000.; }
inline Float_t StPicoBTofPidTraits::btofYLocal() const { return (Float_t)mBTofYLocal / 1000.; }
inline Float_t StPicoBTofPidTraits::btofZLocal() const { return (Float_t)mBTofZLocal / 1000.; }
inline TVector3 StPicoBTofPidTraits::btofHitPos() const { return TVector3((Float_t)mBTofHitPosX / 100., (Float_t)mBTofHitPosY / 100., (Float_t)mBTofHitPosZ / 100.); }
inline Float_t StPicoBTofPidTraits::btofHitPosX() const { return (Float_t)mBTofHitPosX / 100.; }
inline Float_t StPicoBTofPidTraits::btofHitPosY() const { return (Float_t)mBTofHitPosY / 100.; }
inline Float_t StPicoBTofPidTraits::btofHitPosZ() const { return (Float_t)mBTofHitPosZ / 100.; }
#endif
/**
 * Setters
 */
inline void StPicoBTofPidTraits::setTrackIndex(Int_t idx2PicoTrack) { mTrackIndex = (idx2PicoTrack > std::numeric_limits<short>::max()) ? -1 : (Short_t)idx2PicoTrack; }
inline void StPicoBTofPidTraits::setBTofCellId(Int_t tray, Int_t module, Int_t cell) { mBTofCellId  = (Short_t)((tray - 1) * 192 + (module - 1) * 6 + (cell - 1)); }
inline void StPicoBTofPidTraits::setBTofMatchFlag(UChar_t flag) { mBTofMatchFlag = flag; }

#endif
