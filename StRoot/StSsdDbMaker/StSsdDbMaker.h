// $Id: StSsdDbMaker.h,v 1.8 2007/09/25 13:36:55 bouchet Exp $
//
// $Log: StSsdDbMaker.h,v $
// Revision 1.8  2007/09/25 13:36:55  bouchet
// add m_Mode to constructor
//
// Revision 1.7  2007/03/21 17:17:16  fisyak
// use TGeoHMatrix for coordinate transformation, eliminate ssdWafersPostion
//
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
class St_ssdDimensions;
class St_ssdWafersPosition;
class St_slsCtrl;
class slsCtrl_st;
class ssdConfiguration_st;
#include "StSsdUtil/StSsdBarrel.hh"
#include "THashList.h"

class StSsdDbMaker : public StMaker {
 private:
  StSsdBarrel           *mySsd;
  St_ssdDimensions      *m_dimensions;//!
  St_ssdWafersPosition  *m_positions;//!
  ssdConfiguration_st   *m_config;//!
  slsCtrl_st            *m_ctrl;//!
  Int_t                   mode;//!
  static THashList *fRotList;
 public: 
  StSsdDbMaker(const char *name="SsdDb");
  virtual       ~StSsdDbMaker();
  virtual Int_t  Init();
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual void   Clear(const char *opt);
  virtual THashList *GetRotations() {return fRotList;}
  virtual St_ssdWafersPosition *CalculateWafersPosition();
  virtual StSsdBarrel  *GetSsd() {return mySsd;}
  virtual slsCtrl_st   *GetSlsCtrl() {return m_ctrl;}
  virtual Int_t        GetMode(){return mode;}
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSsdDbMaker.h,v 1.8 2007/09/25 13:36:55 bouchet Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  ClassDef(StSsdDbMaker,0)   //StAF chain virtual base class for Makers
};
// Global pointers:
R__EXTERN StSsdDbMaker* gStSsdDbMaker;
#endif


