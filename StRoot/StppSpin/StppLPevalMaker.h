//*-- Author : Jan Balewski
//  
// $Id: StppLPevalMaker.h,v 1.2 2003/01/02 22:19:45 balewski Exp $
// $Log: StppLPevalMaker.h,v $
// Revision 1.2  2003/01/02 22:19:45  balewski
// cleanup #1 of unused code, all makers are now empty
//
// Revision 1.1  2001/04/12 15:19:08  balewski
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
//   Obsolete
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StppLPEvalMaker
#define STAR_StppLPEvalMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StppLPevalMaker : public StMaker 
{
 private: 
  // static Char_t  m_VersionCVS = "$Id: StppLPevalMaker.h,v 1.2 2003/01/02 22:19:45 balewski Exp $";

 protected:

 public: 
  StppLPevalMaker(const char *name="ppLPeval0");
  virtual       ~StppLPevalMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual void Clear(const char *opt);
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StppLPevalMaker.h,v 1.2 2003/01/02 22:19:45 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StppLPevalMaker, 0)   //StAF chain virtual base class for Makers
};
    
#endif
    



