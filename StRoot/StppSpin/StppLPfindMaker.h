//*-- Author : Jan Balewski
//  
// $Id: StppLPfindMaker.h,v 1.8 2003/01/03 23:37:18 balewski Exp $
// $Log: StppLPfindMaker.h,v $
// Revision 1.8  2003/01/03 23:37:18  balewski
// cleanup
//
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//   Temporary test Maker for EEMC code
//   use DST as input
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StSpinPPMaker
#define STAR_StSpinPPMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif


class StppLPfindMaker : public StMaker 
{
 private: 

  // static Char_t  m_VersionCVS = "$Id: StppLPfindMaker.h,v 1.8 2003/01/03 23:37:18 balewski Exp $";
 protected:

 public: 
  StppLPfindMaker(const char *name="ppLPfind0");
  virtual       ~StppLPfindMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual Int_t  Make();

  virtual Int_t InitRun  (int runumber);
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StppLPfindMaker.h,v 1.8 2003/01/03 23:37:18 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StppLPfindMaker, 0)   //StAF chain virtual base class for Makers
};
    
#endif
    



