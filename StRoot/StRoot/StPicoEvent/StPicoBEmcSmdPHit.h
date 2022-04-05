/**
 * \class StPicoBEmcSmdPHit
 * \brief Holds BEMC SmdPhi hit information
 *
 * Holds BEMC SmdPhi hit information
 */

#ifndef StPicoBEmcSmdPHit_h
#define StPicoBEmcSmdPHit_h

#include <limits>

// ROOT headers
#include "TObject.h"

//_________________
class StPicoBEmcSmdPHit: public TObject {

 public:
  /// Default constructor
  StPicoBEmcSmdPHit();
  /// Constructor that fills the parameters accordingly to the input
  StPicoBEmcSmdPHit(Int_t id, Int_t adc, Float_t energy);
  /// Copy constructor
  StPicoBEmcSmdPHit(const StPicoBEmcSmdPHit &hit);
  /// Destructor
  virtual ~StPicoBEmcSmdPHit();
  /// Print BEMC SmdPhi hit information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// SMD Phi hit id
  Int_t id() const        { return (Int_t)mId; }
  /// SMD Phi hit ADC
  Int_t adc() const       { return (Int_t)mAdc; }
  /// SMD Phi hit energy
  Float_t energy() const  { return mEnergy; }

  //
  // Setters
  //

  /// Set hit id
  void setId(Int_t id)    { mId = (id > std::numeric_limits<short>::max()) ? -1 : (Short_t)id; }
  /// Set hit ADC
  void setAdc(Int_t adc)
  { mAdc = ( adc > std::numeric_limits<unsigned short>::max() ) ?
      std::numeric_limits<unsigned short>::max() : (UShort_t)adc; }
  /// Set energy corresponding to the hit
  void setEnergy(Float_t energy) { mEnergy = energy; }

 private:

  /// SMD softId
  Short_t  mId;
  /// ADC of SMD hit
  Short_t  mAdc;
  /// Energy of smd hit
  Float_t mEnergy;

  ClassDef(StPicoBEmcSmdPHit, 1);
};

#endif
