// $Id: StFgtDbMaker.h,v 1.3 2011/10/06 19:01:44 balewski Exp $
/* \class StFgtDbMaker        
\author Stephen Gliske

*/
// jan 12
#ifndef STFGTDBMAKER_H
#define STFGTDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StFgtDbMaker : public StMaker {
 private:

 public: 
  StFgtDbMaker(const char *name="FgtDb");
  virtual       ~StFgtDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtDbMaker.h,v 1.3 2011/10/06 19:01:44 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StFgtDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif

// $Log: StFgtDbMaker.h,v $
// Revision 1.3  2011/10/06 19:01:44  balewski
// *** empty log message ***
//
// Revision 1.2  2011/10/04 02:59:34  balewski
// added guestimates of gains, grid absorption, charge sharing
//
