#ifndef STAR_StSpinSortMaker
#define STAR_StSpinSortMaker

//////////////////////////////////////////////////////////////////////////
//  Obsolete
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StSpinSortMaker : public StMaker {
 private: //....................................................
  
  
 protected: //....................................................
  
 public:  //....................................................

  StSpinSortMaker(const char *name);
  virtual       ~StSpinSortMaker();
  virtual Int_t Init();
  virtual Int_t  Make(); 
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  // static Char_t  m_VersionCVS = "$Id: StSpinSortMaker.h,v 1.4 2003/01/03 23:37:17 balewski Exp $";
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpinSortMaker.h,v 1.4 2003/01/03 23:37:17 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StSpinSortMaker, 0)   //StAF chain virtual base class for Makers
    };
    
#endif
    
    
    
