
#ifndef STFGTNAIVEDBMAKER_H
#define STFGTNAIVEDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StFgtDbMaker.h"

class StFgtNaiveDbMaker : public StFgtDbMaker {

 public: 

  virtual double gain(double locX, double locY, int iDisk);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StFgtNaiveDbMaker.h,v 1.1.1.1 2011/10/03 03:46:57 rfatemi Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StFgtNaiveDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif

