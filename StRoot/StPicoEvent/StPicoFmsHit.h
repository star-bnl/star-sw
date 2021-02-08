#ifndef StPicoFmsHit_h
#define StPicoFmsHit_h

/// ROOT headers
#include "TObject.h"

/**
 * \author: Peifeng Liu, Stony Brook University, pliuphys@gmail.com
 */

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

  /**
   * Getters
   */
  Int_t detectorId() const;
  Int_t channel() const;
  Int_t adc() const;

  /**
   * Setters
   */
  void setChannelDetectorId(Int_t channelId, Int_t detectorId);
  void setAdc(Float_t adc);
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
