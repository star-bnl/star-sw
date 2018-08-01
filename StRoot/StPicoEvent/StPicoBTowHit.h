#ifndef StPicoBTowHit_h
#define StPicoBTowHit_h

/// C++ headers
#include <limits>

/// ROOT headers
#include <TObject.h>
#include <TMath.h>

//_________________
class StPicoBTowHit : public TObject {

 public:
  /// Default constructor
  StPicoBTowHit();
  /// Constructor that takes ADC and energy
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
  Int_t   adc() const;
  Float_t energy() const;
  Bool_t  isBad() const;
  Int_t   numericIndex2SoftId(Int_t idx) const;

  /**
   * Setters
   */
  void setAdc(Int_t adc);
  void setEnergy(Float_t energy);

 protected:

  /// ADC
  UShort_t mAdc;
  /// Energy
  Float16_t mE; //[-5,35,16]

  ClassDef(StPicoBTowHit, 3)
};

/**
 * Setters
 */
inline void StPicoBTowHit::setEnergy(Float_t energy) { mE = energy; }

/**
 * Getters
 */
inline Int_t   StPicoBTowHit::adc() const { return (Int_t)mAdc; }
inline Float_t StPicoBTowHit::energy() const { return mE; }
inline Int_t StPicoBTowHit::numericIndex2SoftId(Int_t idx) const { return (idx+1); }

#endif
