// $Id: StEEsoloPi0Maker.h,v 1.1 2004/04/14 17:09:09 balewski Exp $

#ifndef STAR_StEEsoloPi0Maker
#define STAR_StEEsoloPi0Maker

/*!
 *                                                                     
 * \class  StEEsoloPi0Maker
 * \author Balewski
 * \date   
 * \brief  
 *
 Axample to access muDst and pass it to ezTree analyzis class
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class TObjArray  ;
#include "EEsoloPi0.h"
class StMuDstMaker;

class StEEsoloPi0Maker : public StMaker, public  EEsoloPi0 {
 private:
  StMuDstMaker* mMuDstMaker;
  TObjArray  *HList; 
  int runID; 
  int off48;
  TString treeName;

  int getAdc();
 public: 
  StEEsoloPi0Maker(const char *self="stEEsoloPi0", const char* muDstMakerName="muDstMaker");
  virtual       ~StEEsoloPi0Maker();
  virtual Int_t Init();
  virtual Int_t Finish();

  virtual Int_t  Make();
  void SetOutDir(const char * path) {treeName=path;}
  void Set(TObjArray * x){HList=x;} 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEsoloPi0Maker.h,v 1.1 2004/04/14 17:09:09 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StEEsoloPi0Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StEEsoloPi0Maker.h,v $
// Revision 1.1  2004/04/14 17:09:09  balewski
// new copy of pi0finder with towers only, should work on ezTree as well (after small cleanup)
//
