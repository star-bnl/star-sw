//
//*-- Author : Jan Balewski 
// $Id: StppLMVevalMaker.h,v 1.1 2001/04/23 21:47:17 balewski Exp $
// $Log: StppLMVevalMaker.h,v $
// Revision 1.1  2001/04/23 21:47:17  balewski
// *** empty log message ***
//
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
//  Emulates trigger response for the M-C data
//  May work active (m_mode==1) or passive (m_mode=0=default) 
//                                                                     
//////////////////////////////////////////////////////////////////////////
#ifndef STAR_StppLMVevalMaker
#define STAR_StppLMVevalMaker
   
//////////////////////////////////////////////////////////////////////////
//                                                              
// StppLMVevalMaker : evaluate ppLMVeval
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StEvent;

#include "TH2.h"// for histos
class TH1F;
class TH2F;

class StppLMVevalMaker : public StMaker 
{  
 
 private: //....................................................
  Int_t Finish();

  TH1F *he[16];
  void getGenerated( int&);

  // static Char_t  m_VersionCVS = "$Id: StppLMVevalMaker.h,v 1.1 2001/04/23 21:47:17 balewski Exp $";
 
  // static Char_t  m_VersionCVS = "$Id: StppLMVevalMaker.h,v 1.1 2001/04/23 21:47:17 balewski Exp $";

 protected:  //....................................................
  
 public: //....................................................
   StppLMVevalMaker(const char *name="ppLMVeval");
   virtual       ~StppLMVevalMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: StppLMVevalMaker.h,v 1.1 2001/04/23 21:47:17 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   
   ClassDef(StppLMVevalMaker, 0)   //StAF chain virtual base class for Makers
};

#endif
