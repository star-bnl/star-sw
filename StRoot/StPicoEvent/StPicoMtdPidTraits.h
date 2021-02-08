#ifndef StPicoMtdPidTraits_h
#define StPicoMtdPidTraits_h

/// ROOT headers
#include "TObject.h"

//_________________
class StPicoMtdPidTraits : public TObject {

 public:
  
  /// Default constructor
  StPicoMtdPidTraits();
  /// Copy constructor
  StPicoMtdPidTraits(const StPicoMtdPidTraits &traits);
  /// Destructor
  virtual ~StPicoMtdPidTraits();
  /// Print MTD PID traits information
  virtual void Print(const Char_t* option = "") const;

  /**
   * Getters
   */
  /// Matching information
  Int_t    trackIndex()        const;
  Int_t    mtdHitIndex()       const;
  Int_t    gChannel()          const;
  Int_t    backleg()           const;
  Int_t    module()            const;
  Int_t    cell()              const;
  Int_t    matchFlag()         const;
  Float_t  deltaY()            const;
  Float_t  deltaZ()            const;
  Float_t  deltaTimeOfFlight() const;
  Float_t  beta()              const;

  /**
   * Setters
   */
  void setTrackIndex(Int_t index);
  void setMtdHitIndex(Int_t index);
  void setMatchFlag(Char_t flag);
  void setDeltaY(Float_t dy);
  void setDeltaZ(Float_t dz);
  void setDeltaTimeOfFlight(Float_t t);
  void setBeta(Float_t beta);
  void setHitChannel(Int_t backleg, Int_t module, Int_t cell);

 private:

  /// Index to the associated track in the event
  Short_t   mTrackIndex;
  /// Index to the associated MTD hit in the event
  Short_t   mMtdHitIndex;
  /// Matching flag indicating multiple matches
  Char_t    mMatchFlag;
  /// DeltaY between matched track-hit pair * 200 = provides 0.005 cm precision
  Short_t   mDeltaY;
  /// DeltaZ between matched track-hit pair * 200 = provides 0.005 cm precision
  Short_t   mDeltaZ;
  /// Difference between measured and expected time-of-flight
  Float_t   mDeltaTimeOfFlight;
  /// Beta of matched tracks * 20000
  UShort_t  mBeta;
  /// HitChan encoding: (backleg-1) * 60 + (module-1) * 12 + cell
  Short_t   mMtdHitChan;

  ClassDef(StPicoMtdPidTraits, 3)
};

/**
 * Getters
 */
inline Int_t    StPicoMtdPidTraits::trackIndex()        const { return mTrackIndex; }
inline Int_t    StPicoMtdPidTraits::mtdHitIndex()       const { return mMtdHitIndex; }
inline Int_t    StPicoMtdPidTraits::gChannel()          const { return mMtdHitChan; }
inline Int_t    StPicoMtdPidTraits::backleg()           const { return mMtdHitChan / 60 + 1; }
inline Int_t    StPicoMtdPidTraits::module()            const { return (mMtdHitChan % 60) / 12 + 1; }
inline Int_t    StPicoMtdPidTraits::cell()              const { return mMtdHitChan % 12; }
inline Int_t    StPicoMtdPidTraits::matchFlag()         const { return mMatchFlag; }
inline Float_t  StPicoMtdPidTraits::deltaY()            const { return (Float_t)mDeltaY / 200.; }
inline Float_t  StPicoMtdPidTraits::deltaZ()            const { return (Float_t)mDeltaZ / 200.; }
inline Float_t  StPicoMtdPidTraits::deltaTimeOfFlight() const { return mDeltaTimeOfFlight; }
inline Float_t  StPicoMtdPidTraits::beta()              const { return (Float_t)mBeta / 20000.; }

/**
 * Setters
 */
inline void    StPicoMtdPidTraits::setTrackIndex(Int_t index) { mTrackIndex = (Short_t) index; }
inline void    StPicoMtdPidTraits::setMtdHitIndex(Int_t index) { mMtdHitIndex = (Short_t) index; }
inline void    StPicoMtdPidTraits::setMatchFlag(Char_t flag) { mMatchFlag = (Char_t)flag; }
inline void    StPicoMtdPidTraits::setDeltaTimeOfFlight(Float_t t) { mDeltaTimeOfFlight = t; }

#endif
