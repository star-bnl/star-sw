//*-- Author : Jan Balewski
//  
// $Id: StppLPprojectMaker.h,v 1.6 2003/01/03 23:37:18 balewski Exp $
// $Log: StppLPprojectMaker.h,v $
// Revision 1.6  2003/01/03 23:37:18  balewski
// cleanup
//
// Revision 1.5  2003/01/02 22:19:45  balewski
// cleanup #1 of unused code, all makers are now empty
//
// Revision 1.4  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:13  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Obsolete Maker
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//
#ifndef STAR_StppLPprojectMaker
#define STAR_StppLPprojectMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StppLPprojectMaker : public StMaker {
 private:
  // static Char_t  m_VersionCVS = "$Id: StppLPprojectMaker.h,v 1.6 2003/01/03 23:37:18 balewski Exp $";

 protected:
 public: 
  StppLPprojectMaker(const char *name);
  virtual       ~StppLPprojectMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t Finish(); 
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: StppLPprojectMaker.h,v 1.6 2003/01/03 23:37:18 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   
   ClassDef(StppLPprojectMaker, 0)   //StAF chain virtual base class for Makers
};

#endif


