//*-- Author : Jan Balewski
//  
// JB 3/30/01 - divorce with MC. Only StEvent is used. No evaluation
//
// $Id: StppLPfindMaker.h,v 1.7 2003/01/02 22:19:45 balewski Exp $
// $Log: StppLPfindMaker.h,v $
// Revision 1.7  2003/01/02 22:19:45  balewski
// cleanup #1 of unused code, all makers are now empty
//
// Revision 1.6  2001/06/07 17:02:53  balewski
// *** empty log message ***
//
// Revision 1.5  2001/04/26 20:04:52  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
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
  // static Char_t  m_VersionCVS = "$Id: StppLPfindMaker.h,v 1.7 2003/01/02 22:19:45 balewski Exp $";
 protected:

 public: 
  StppLPfindMaker(const char *name="ppLPfind0");
  virtual       ~StppLPfindMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StppLPfindMaker.h,v 1.7 2003/01/02 22:19:45 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StppLPfindMaker, 0)   //StAF chain virtual base class for Makers
};
    
#endif
    



