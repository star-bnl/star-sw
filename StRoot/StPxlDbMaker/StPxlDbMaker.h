// $Id: StPxlDbMaker.h,v 1.2 2013/05/29 18:12:16 bouchet Exp $
// $Log: StPxlDbMaker.h,v $
// Revision 1.2  2013/05/29 18:12:16  bouchet
// added GetCVS tags
//
// Revision 1.1  2013/05/24 15:59:53  bouchet
// first version
//
/***************************************************************************
 * Author:  J. Bouchet, M. Lomnitz , KSU
 * Description: PXL DB access Maker
 **************************************************************************/

#ifndef STPXLDBMAKER_H
#define STPXLDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "THashList.h"
class StPxlDbMaker : public StMaker {
 private:
  static THashList *fRotList;

 public: 
  StPxlDbMaker(const char *name="PxlDb");
  virtual       ~StPxlDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt);
  virtual THashList *GetRotations() {return fRotList;}
  virtual Int_t CalculateSensorsPosition();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StPxlDbMaker.h,v 1.2 2013/05/29 18:12:16 bouchet Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StPxlDbMaker,0)   //StAF chain virtual base class for Makers
};
// Global pointers:
R__EXTERN StPxlDbMaker* gStPxlDbMaker;
#endif


