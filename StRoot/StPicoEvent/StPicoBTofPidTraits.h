#ifndef StPicoBTofPidTraits_h
#define StPicoBTofPidTraits_h

/// C++ headers
#include <limits>

/// ROOT headers
#include <TObject.h>
#include <TVector3.h>
#include <TBufferFile.h>

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
  
  Float_t btof() const;
  Float_t btofBeta() const;
  Float_t btofYLocal() const;
  Float_t btofZLocal() const;
  
  TVector3 btofHitPos() const;
  
  Float_t btofHitPosX() const;
  Float_t btofHitPosY() const;
  Float_t btofHitPosZ() const;

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

  /// Index to the associated picoTrack in the event
  Short_t  mTrackIndex;
  /// CellId encodes: (tray-1)*192+(module-1)*6+(cell-1): -1 - no match
  Short_t  mBTofCellId;
  /// 0 - no match, 1 - one-to-one, 2 - one-to-multiple
  UChar_t  mBTofMatchFlag;
  /// Time-Of-Flight (UShort_t or Float16_t with specification is not enough)
  Float16_t mBTof;
  /// Beta
  Float16_t mBTofBeta;    //[0,3,16]
  /// ylocal
  Float16_t mBTofYLocal;  //[-6,6,16]
  /// zlocal
  Float16_t mBTofZLocal;  //-4,4,16]
  /// Projected hit positions
  Float16_t  mBTofHitPosX; //[-240,240,16]
  Float16_t  mBTofHitPosY; //[-240,240,16]
  Float16_t  mBTofHitPosZ; //[-240,240,16]

  ClassDef(StPicoBTofPidTraits, 2);
};

/**
 * Getters
 */
inline Int_t   StPicoBTofPidTraits::trackIndex() const { return mTrackIndex; }
inline Int_t   StPicoBTofPidTraits::btofCellId() const { return (Int_t)mBTofCellId; }
inline Int_t   StPicoBTofPidTraits::btofMatchFlag() const { return (Int_t)mBTofMatchFlag; }
inline Float_t StPicoBTofPidTraits::btof() const { return mBTof; }
inline Float_t StPicoBTofPidTraits::btofBeta() const { return mBTofBeta; }
inline Float_t StPicoBTofPidTraits::btofYLocal() const { return mBTofYLocal; }
inline Float_t StPicoBTofPidTraits::btofZLocal() const { return mBTofZLocal; }
inline TVector3 StPicoBTofPidTraits::btofHitPos() const { return TVector3(mBTofHitPosX, mBTofHitPosY, mBTofHitPosZ); }
inline Float_t StPicoBTofPidTraits::btofHitPosX() const { return mBTofHitPosX; }
inline Float_t StPicoBTofPidTraits::btofHitPosY() const { return mBTofHitPosY; }
inline Float_t StPicoBTofPidTraits::btofHitPosZ() const { return mBTofHitPosZ; }

/**
 * Setters
 */
inline void StPicoBTofPidTraits::setTrackIndex(Int_t idx2PicoTrack) 
{ mTrackIndex = (idx2PicoTrack > std::numeric_limits<short>::max()) ? -1 : (Short_t)idx2PicoTrack; }
inline void StPicoBTofPidTraits::setTOF(Float_t tof) { mBTof = tof; }
inline void StPicoBTofPidTraits::setBeta(Float_t beta) { mBTofBeta = beta; }
inline void StPicoBTofPidTraits::setHitPositionX(Float_t x) { mBTofHitPosX = x; }
inline void StPicoBTofPidTraits::setHitPositionY(Float_t y) { mBTofHitPosY = y; }
inline void StPicoBTofPidTraits::setHitPositionZ(Float_t z) { mBTofHitPosZ = z; }
inline void StPicoBTofPidTraits::setHitPositionXYZ(Float_t x, Float_t y, Float_t z)
{ mBTofHitPosX = x; mBTofHitPosY = y; mBTofHitPosZ = z; }
inline void StPicoBTofPidTraits::setYLocal(Float_t yLocal) { mBTofYLocal = yLocal; }
inline void StPicoBTofPidTraits::setZLocal(Float_t zLocal) { mBTofZLocal = zLocal; }
inline void StPicoBTofPidTraits::setBTofCellId(Int_t tray, Int_t module, Int_t cell) 
{ mBTofCellId  = (Short_t)((tray - 1) * 192 + (module - 1) * 6 + (cell - 1)); }
inline void StPicoBTofPidTraits::setBTofMatchFlag(UChar_t flag) { mBTofMatchFlag = flag; }

#endif
