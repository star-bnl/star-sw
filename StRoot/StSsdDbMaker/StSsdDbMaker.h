// $Id: StSsdDbMaker.h,v 1.6 2006/10/16 19:53:24 fisyak Exp $
//
// $Log: StSsdDbMaker.h,v $
// Revision 1.6  2006/10/16 19:53:24  fisyak
// Adjust for new Ssd chain
//
// Revision 1.5  2005/06/20 14:21:38  lmartin
// CVS tags added
//

/***************************************************************************
 * Author: christelle roy
 * Description: SSD DB access Maker
 **************************************************************************/

#ifndef STSSDDBMAKER_H
#define STSSDDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StSsdDbMaker : public StMaker {

 public: 
  StSsdDbMaker(const char *name="SsdDb") : StMaker(name) {}
  virtual       ~StSsdDbMaker() {}
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSsdDbMaker.h,v 1.6 2006/10/16 19:53:24 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StSsdDbMaker,0)   //StAF chain virtual base class for Makers
};
#endif


