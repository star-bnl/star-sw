/**
 * \class StPicoFmsHit
 * \brief Holds information about FMS hit
 *
 * The class keeps information about the hit from Forward Meson Spectrometer
 * 
 * \author Peifeng Liu, Stony Brook University, pliuphys@gmail.com
 */

#ifndef StPicoFmsHit_h
#define StPicoFmsHit_h

// ROOT headers
#include "TObject.h"

//_________________
class StPicoFmsHit: public TObject {

 public:
  /// Default constructor
  StPicoFmsHit();
  /// Constructor that takes values
  StPicoFmsHit(Int_t detectorId, Int_t channelId, Int_t adc);
  /// Copy constructor
  StPicoFmsHit(const StPicoFmsHit &hit);
  /// Destructor
  virtual ~StPicoFmsHit();
  /// Prints FMS hit information
  virtual void Print(const Char_t *option = "") const;

  //
  // Getters
  //
  
  /// Return detector ID [0,31]
  Int_t detectorId() const;
  /// Return channel [0,2047]
  Int_t channel() const;
  /// Return ADC
  Int_t adc() const;

  //
  // Setters
  //

  /// Set channel and detector ID
  void setChannelDetectorId(Int_t channelId, Int_t detectorId);
  /// Set ADC
  void setAdc(Float_t adc);
  /// Set ADC
  void setAdc(Int_t adc);
  
 private:

  /// 32*channel + detectorId. Allow channel 0-2047, detectorId 0-31.
  UShort_t mChannelDetectorId;
  /// ADC
  UShort_t mAdc;

  ClassDef(StPicoFmsHit, 1)
};

inline Int_t StPicoFmsHit::detectorId() const { return (Int_t)( mChannelDetectorId % 32 ); }
inline Int_t StPicoFmsHit::channel() const { return (Int_t) (mChannelDetectorId / 32); }
inline Int_t StPicoFmsHit::adc() const { return (Int_t)mAdc; }

#endif
