#ifndef StPicoFmsHit_h
#define StPicoFmsHit_h

#include "TObject.h"
#include "StarClassLibrary/StThreeVectorF.hh"

/**
 * \author: Peifeng Liu, Stony Brook University, pliuphys@gmail.com
 */

//_________________
class StPicoFmsHit: public TObject {

 public:
  //Default constructor
  StPicoFmsHit();
  //Constructor that takes values
  StPicoFmsHit(int detectorId, int channelId, int adc);
  //Copy constructor
  StPicoFmsHit(const StPicoFmsHit &hit);
  //Destructor
  virtual ~StPicoFmsHit();
  //Prints FMS hit information
  virtual void Print(const Char_t *option = "") const;

  int detectorId() const;
  int channel() const;
  int adc() const;

 private:

  /// 32*channel + detectorId. Allow channel 0-2047, detectorId 0-31.
  UShort_t mChannelDetectorId;
  UShort_t mAdc;

  ClassDef(StPicoFmsHit, 1)
};

inline int StPicoFmsHit::detectorId() const { return mChannelDetectorId % 32; }
inline int StPicoFmsHit::channel() const { return mChannelDetectorId / 32; }
inline int StPicoFmsHit::adc() const { return mAdc; }

#endif
