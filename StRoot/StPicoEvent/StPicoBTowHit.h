/**
 * \class StPicoBTowHit
 * \brief Holds information about BEMC tower
 *
 * The class holds information about the tower from
 * the Barrel ElectroMagnetic Calorimeter (BEMC)
 */

#ifndef StPicoBTowHit_h
#define StPicoBTowHit_h

// C++ headers
#include <limits>

// ROOT headers
#include "TObject.h"

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

  //
  // Getters
  //
  
  /// Return ADC of the tower
  Int_t   adc() const;
  /// Return energy of the tower
  Float_t energy() const;
  /// Return if the tower is bad
  Bool_t  isBad() const;
  /// Return softId
  Int_t   numericIndex2SoftId(Int_t idx) const;

  //
  // Setters
  //
  
  /// Set tower ADC
  void setAdc(Int_t adc);
  /// Set tower energy
  void setEnergy(Float_t energy);

 protected:

  /// ADC
  UShort_t mAdc;
  /// Energy * 1000
  Short_t mE;

  ClassDef(StPicoBTowHit, 4)
};

//
// Getters
//
inline Int_t   StPicoBTowHit::adc() const { return (Int_t)mAdc; }
inline Float_t StPicoBTowHit::energy() const { return (Float_t)mE / 1000.f; }
inline Int_t StPicoBTowHit::numericIndex2SoftId(Int_t idx) const { return (idx+1); }

#endif
