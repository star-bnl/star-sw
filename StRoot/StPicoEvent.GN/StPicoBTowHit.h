#ifndef StPicoBTowHit_h
#define StPicoBTowHit_h

/// C++ headers
#include <limits>

/// ROOT headers
#include "TObject.h"
#include "TMath.h"

//_________________
class StPicoBTowHit : public TObject {

 public:
  /// Default constructor
  StPicoBTowHit();
  /// Constructor that takes id, ADC and energy
  //StPicoBTowHit(int id, int adc, float e);
  StPicoBTowHit(Int_t adc, Float_t e);
  /// Copy constructor
  StPicoBTowHit(const StPicoBTowHit &hit);
  /// Destructor
  virtual ~StPicoBTowHit();
  /// Print tower information
  virtual void Print(const Char_t* option = "") const;

  /**
   * Getters
   */
  //Int_t   id() const;
  Int_t   adc() const;
  Float_t energy() const;
  Bool_t  isBad() const;

  /**
   * Setters
   */
  void setAdc(Int_t adc);
  void setEnergy(Float_t energy);

 protected:

  //UShort_t mId;    // towerId 1-4800
  UShort_t mAdc;   // adc
  Short_t  mE;     // Energy * 1000;

  ClassDef(StPicoBTowHit, 2)
};
/**
 * Getters
 */
//inline Int_t   StPicoBTowHit::id() const { return (Int_t)mId; }
inline Int_t   StPicoBTowHit::adc() const { return (Int_t)mAdc; }
inline Float_t StPicoBTowHit::energy() const { return (Float_t)mE / 1000.; }
inline Bool_t  StPicoBTowHit::isBad() const {
  if( energy()==-9. && mAdc==0) {
    return kTRUE;
  }
  else {
    return kFALSE;
  }
}

/**
 * Setters
 */
inline void StPicoBTowHit::setAdc(Int_t adc) {
  if(adc<0) {
    mAdc = 0;
  }
  else {
    mAdc  = (adc > std::numeric_limits<unsigned short>::max()) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)adc;
  }
}
inline void StPicoBTowHit::setEnergy(Float_t e) {
  mE = (e * 1000. > std::numeric_limits<short>::max()) ?
    std::numeric_limits<short>::max() : (Short_t)(TMath::Nint(e * 1000.));
}
#endif
