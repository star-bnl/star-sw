
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

  virtual double gain(double locX, double locY, int iDisk);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtDbMaker.h,v 1.1.1.1 2011/10/03 03:46:57 rfatemi Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StFgtDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif

