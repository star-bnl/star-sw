// $Id: StEEtowerExampleMaker.h,v 1.2 2014/08/06 11:43:01 jeromel Exp $

#ifndef STAR_StEEtowerExampleMaker
#define STAR_StEEtowerExampleMaker

/*!
 *                                                                     
 * \class  StEEtowerExampleMaker
 * \author Balewski
 * \date   
 * \brief  
 *
 Axample to access EEMC data & DB  from muDst in StRoot-framework
 Only muDst data are decoded by this class 
 Uses EEtowers class to do any analysis
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TObjArray  ;
class StMuDstMaker;

#include "EEtower.h"

class StEEtowerExampleMaker : public StMaker , public  EEtower{

 private: 
  StMuDstMaker* mMuDstMaker;  
  int   unpackMuDst();

 public: 
  StEEtowerExampleMaker(const char *self="stEEsoloPi0", const char* muDstMakerName="muDstMaker");
  virtual       ~StEEtowerExampleMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual Int_t  Make();
  void Set(TObjArray * x){HList=x;} 
  
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEtowerExampleMaker.h,v 1.2 2014/08/06 11:43:01 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StEEtowerExampleMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StEEtowerExampleMaker.h,v $
// Revision 1.2  2014/08/06 11:43:01  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2004/06/06 04:54:10  balewski
// dual analyzis
//
// Revision 1.2  2004/04/14 19:34:01  balewski
