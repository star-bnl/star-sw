/**
 * \class StPicoBEmcSmdEHit
 * \brief Holds BEMC SmdEta hit information
 *
 * Holds BEMC SmdEta hit information
 */

#ifndef StPicoBEmcSmdEHit_h
#define StPicoBEmcSmdEHit_h

// ROOT headers
#include "TObject.h"

//_________________
class StPicoBEmcSmdEHit: public TObject {

 public:
  /// Default constructor
  StPicoBEmcSmdEHit();
  /// Constructor that fills the parameters accordingly to the input
  StPicoBEmcSmdEHit(Int_t id, Int_t adc, Float_t energy);
  /// Copy constructor
  StPicoBEmcSmdEHit(const StPicoBEmcSmdEHit &hit);
  /// Destructor
  virtual ~StPicoBEmcSmdEHit();
  /// Print BEMC SmdEta hit information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// SMD Eta hit id
  Int_t id() const;
  /// SMD Eta hit ADC
  Int_t adc() const;
  /// SMD Eta hit energy
  Float_t energy() const;

  //
  // Setters
  //

  /// Set hit id
  void setId(Int_t id);
  /// Set hit ADC
  void setAdc(Int_t adc);
  /// Set energy corresponding to the hit
  void setEnergy(Float_t energy);

 private:

  /// SMD softId
  Short_t  mId;
  /// ADC of SMD hit
  Short_t  mAdc;
  /// Energy of smd hit
  Float_t mEnergy;

  ClassDef(StPicoBEmcSmdEHit, 1);
};

//
// Getters
//
inline Int_t   StPicoBEmcSmdEHit::id() const { return (Int_t)mId; }
inline Int_t   StPicoBEmcSmdEHit::adc() const { return (Int_t)mAdc; }
inline Float_t StPicoBEmcSmdEHit::energy() const { return mEnergy; }

//
// Setters
//
inline void StPicoBEmcSmdEHit::setId(Int_t id)
{ mId = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id; }
inline void StPicoBEmcSmdEHit::setAdc(Int_t adc)
{ mAdc = (adc > std::numeric_limits<unsigned short>::max()) ?
  std::numeric_limits<unsigned short>::max() : (UShort_t)adc; }
inline void StPicoBEmcSmdEHit::setEnergy(Float_t energy) { mEnergy = energy; }
#endif
