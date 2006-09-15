// $Id: StSpaListNoise.hh,v 1.3 2006/09/15 21:09:52 bouchet Exp $
//
// $Log: StSpaListNoise.hh,v $
// Revision 1.3  2006/09/15 21:09:52  bouchet
// read the noise and pedestal from ssdStripCalib
//
// Revision 1.2  2005/05/13 08:39:32  lmartin
// CVS tags added
//

#ifndef STSPALISTNOISE_HH
#define STSPALISTNOISE_HH
#include <stdlib.h>
#include <math.h>
#include "StSpaListStrip.hh"
#include "StSpaNoise.hh"

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
			    long nElectronInAMip,long a128Dynamic);
  void            substractPedestal();
  void            convertAnalogToDigit(long nElectronInAMip,long adcDynamic,
				       long nbitEncoding,float daqCutValue);
  void            zeroSubstraction();
  
private:
  int         mListLength;
  StSpaNoise *mFirstS;
  StSpaNoise *mLastS;
};
#endif
