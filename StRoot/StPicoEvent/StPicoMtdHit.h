#ifndef StPicoMtdHit_h
#define StPicoMtdHit_h

#include <utility>
#include "TObject.h"
class StMuMtdHit;

//_________________
class StPicoMtdHit : public TObject {

 public:

  StPicoMtdHit();
  //Main constructor that takes the Mtd from MuDst
  StPicoMtdHit(StMuMtdHit const* hit);
  //Copy constructor
  StPicoMtdHit(const StPicoMtdHit &hit);
  //Destructor
  virtual ~StPicoMtdHit();
  //Print the stored information
  virtual void Print(const Char_t* option = "") const;

  void  setTriggerFlag(Int_t const flag);

  //Return global channel number of the MTD hit
  //Its defition is (backleg-1) * 60 + (module-1) * 12 + cell
  Int_t  gChannel()    const;

  //Return backleg number (1-30) of the MTD hit
  Int_t  backleg()     const;

  //Return module number (1-5) of the MTD hit
  Int_t  module()      const;

  //Return cell number (0-11) of the MTD hit
  Int_t  cell()        const;

  //Return trigger flag of the MTD hit. The returned value 
  //indicates the number of MTD hits matched to the same 
  //trigger unit that fired the MTD trigger.
  Int_t  triggerFlag() const;

  std::pair<Float_t, Float_t> leadingEdgeTime()  const;
  std::pair<Float_t, Float_t> trailingEdgeTime() const;

  /// Return time-over-threshold of the MTD hit
  std::pair<Float_t, Float_t> tot() const;

 protected:

  Short_t mgChannel;    ///< mgChannel = (backleg-1) * 60 + (module-1) * 12 + cell
  UChar_t mTriggerFlag; ///< # of hits in the corresponding trigger unit that fired the trigger
  std::pair<Float_t, Float_t>  mLeadingEdgeTime; ///< Leading-edge time for the hit
  std::pair<Float_t, Float_t>  mTrailingEdgeTime;  ///< Trailing-edge time for the hit

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
