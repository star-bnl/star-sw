#ifndef STSPALISTNOISE_HH
#define STSPALISTNOISE_HH
# include <stdiostream.h>
# include <stdlib.h>
# include <math.h>
# include "StSpaListStrip.hh"
# include "StSpaNoise.hh"

class StSpaListStrip;
class StSpaListNoise
{
 public:
  StSpaListNoise();
  ~StSpaListNoise();
  StSpaNoise*     next(StSpaNoise *ptr);
  StSpaNoise*     prev(StSpaNoise *ptr);
  StSpaNoise*     first();
  StSpaNoise*     last();
  int             addNewNoise(StSpaNoise *ptr);
  void            setIsActive(int rIsActive, int rNStrip);
  StSpaListNoise* addListNoise(StSpaListNoise *list);
  void            exchangeTwoNoise(StSpaNoise *ptr1, StSpaNoise *ptr2);
  void            sortStrip();
  int             removeNoise(StSpaNoise *ptr);
  int             getSize();
  void            addSignal(StSpaListStrip *ptr,
			    long NElectronInAMip,long A128Dynamic);
  void            substractPedestal();
  void            convertAnalogToDigit(long NElectronInAMip,long ADCDynamic,
				       long NBitEncoding,float DAQCutValue);
  void            zeroSubstraction();

private:
  int         mListLength;
  StSpaNoise *mFirstS;
  StSpaNoise *mLastS;
};
#endif
