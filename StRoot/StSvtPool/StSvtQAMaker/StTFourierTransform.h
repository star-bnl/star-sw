#ifndef __StTFourierTransform_h__
#define __StTFourierTransform_h__

#include "TObject.h"
#include "TH1.h"

class StTFourierTransform : public TObject
{ 

  // This class is not meant to be instantiated; it is simply a module

  public:

  void Fft(TH1 * in, TH1 * amplitude, TH1 * phase = 0, UInt_t start = 0,
	   UInt_t end = 0);

  ClassDef(StTFourierTransform, 0) // Fast Fourier transform
};

#endif
