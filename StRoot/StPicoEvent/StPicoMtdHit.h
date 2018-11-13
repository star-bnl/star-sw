/**
 * \class StPicoMtdHit
 * \brief Holds information about the hit from MTD
 *
 * The class holds information about the hit from the Muon Telescope Detector (MTD)
 */

#ifndef StPicoMtdHit_h
#define StPicoMtdHit_h

// C++ headers
#include <utility>

// ROOT headers
#include "TObject.h"

//_________________
class StPicoMtdHit : public TObject {

 public:

  /// Default constructor
  StPicoMtdHit();
  /// Copy constructor
  StPicoMtdHit(const StPicoMtdHit &hit);
  /// Destructor
  virtual ~StPicoMtdHit();
  /// Print the stored information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// Return global channel number of the MTD hit
  /// Its defition is (backleg-1) * 60 + (module-1) * 12 + cell
  Int_t  gChannel()    const;

  /// Return backleg number (1-30) of the MTD hit
  Int_t  backleg()     const;

  /// Return module number (1-5) of the MTD hit
  Int_t  module()      const;

  /// Return cell number (0-11) of the MTD hit
  Int_t  cell()        const;

  /// Return trigger flag of the MTD hit. The returned value 
  /// indicates the number of MTD hits matched to the same 
  /// trigger unit that fired the MTD trigger.
  Int_t  triggerFlag() const;

  /// Return leading edge time
  std::pair<Float_t, Float_t> leadingEdgeTime()  const;

  /// Return trailing edge time
  std::pair<Float_t, Float_t> trailingEdgeTime() const;

  /// Return time-over-threshold of the MTD hit
  std::pair<Float_t, Float_t> tot() const;

  //
  // Setters
  //

  /// Set trigger flag of the MTD hit (corresponds to the number)
  /// of MTD hits matchedd to the same trigger unit that fired
  /// MTD trigger
  void  setTriggerFlag(Int_t const flag);

  /// Set leading edge time
  void  setLeadingEdgeTime(std::pair<Float_t, Float_t> leadingEdgeTime);

  /// Set trailing edge time
  void  setTrailingEdgeTime(std::pair<Float_t, Float_t> trailingEdgeTime);

  /// Encode global channel number as: (backleg-1) * 60 + (module-1) * 12 + cell
  void  setHitChannel(Int_t backleg, Int_t module, Int_t cell);

 protected:

  /// Encoding: mgChannel = (backleg-1) * 60 + (module-1) * 12 + cell
  Short_t mgChannel;
  /// Number of hits in the corresponding trigger unit that fired the trigger
  UChar_t mTriggerFlag;
  /// Leading-edge time for the hit
  std::pair<Float_t, Float_t>  mLeadingEdgeTime;
  /// Trailing-edge time for the hit
  std::pair<Float_t, Float_t>  mTrailingEdgeTime;

  ClassDef(StPicoMtdHit, 1)
};

inline void StPicoMtdHit::setTriggerFlag(Int_t const flag) { mTriggerFlag = (UChar_t)flag; }
inline Int_t StPicoMtdHit::gChannel()    const { return (Int_t)mgChannel; }
inline Int_t StPicoMtdHit::backleg()     const { return (Int_t)mgChannel / 60 + 1; }
inline Int_t StPicoMtdHit::module()      const { return ((Int_t)mgChannel % 60) / 12 + 1; }
inline Int_t StPicoMtdHit::cell()        const { return (Int_t)mgChannel % 12; }
inline Int_t StPicoMtdHit::triggerFlag() const { return (Int_t) mTriggerFlag; }
inline std::pair<Float_t, Float_t> StPicoMtdHit::leadingEdgeTime()  const { return mLeadingEdgeTime; }
inline std::pair<Float_t, Float_t> StPicoMtdHit::trailingEdgeTime() const { return mTrailingEdgeTime; }
inline std::pair<Float_t, Float_t> StPicoMtdHit::tot() const {
  return std::pair<Float_t, Float_t>(mTrailingEdgeTime.first - mLeadingEdgeTime.first, mTrailingEdgeTime.second - mLeadingEdgeTime.second);
}

#endif
