// $Id: StSpaListNoise.hh,v 1.1 2015/06/23 16:26:19 jeromel Exp $
//
// $Log: StSpaListNoise.hh,v $
// Revision 1.1  2015/06/23 16:26:19  jeromel
// First version created from the SSD code and reshaped
//
// Revision 1.1  2015/04/19 17:30:31  bouchet
// initial commit ; SST codes
//
// Revision 1.1  2015/01/29 20:16:36  bouchet
// SSD utils for hit reconstruction
//
// Revision 1.2  2009/02/23 21:10:40  bouchet
// increase NSaturationSignal to reflect the energy increase of the GEANT hit
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSstUtil regroups now methods for the classes StSstStrip, StSstCluster and StSstPoint
//
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
#include "Rtypes.h"
#include "StSpaNoise.hh"
#include "StSstStripList.hh"

class StSpaListNoise
{
 public:
  StSpaListNoise();
  ~StSpaListNoise();
  StSpaNoise*     next(StSpaNoise *ptr);
  StSpaNoise*     prev(StSpaNoise *ptr);
  StSpaNoise*     first();
  StSpaNoise*     last();
  Int_t           addNewNoise(StSpaNoise *ptr);
  void            setIsActive(Int_t rIsActive, Int_t rNStrip);
  StSpaListNoise* addListNoise(StSpaListNoise *list);
  void            exchangeTwoNoise(StSpaNoise *ptr1, StSpaNoise *ptr2);
  void            sortStrip();
  Int_t           removeNoise(StSpaNoise *ptr);
  Int_t           getSize();
  void            addSignal(StSstStripList *ptr, Long_t nElectronInAMip,Long_t adcDynamic);
  void            substractPedestal();
  void            convertAnalogToDigit(Long_t nElectronInAMip,Long_t adcDynamic,
				       Long_t nbitEncoding,Float_t daqCutValue);
  void            zeroSubstraction();
  
private:
  Int_t         mListLength;
  StSpaNoise *mFirstS;
  StSpaNoise *mLastS;
};
#endif
