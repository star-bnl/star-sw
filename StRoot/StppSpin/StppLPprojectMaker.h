//*-- Author : Jan Balewski
//  
// $Id: StppLPprojectMaker.h,v 1.1.1.2 2001/04/21 00:43:14 fisyak Exp $
// $Log: StppLPprojectMaker.h,v $
// Revision 1.1.1.2  2001/04/21 00:43:14  fisyak
// *** empty log message ***
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
// Projects events in tho phi/pT bins depending on spin bits            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
//
#ifndef STAR_StppLPprojectMaker
#define STAR_StppLPprojectMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StppLPprojectMaker virtual base class for Maker                       
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

#define MxSpinID 4

class StppLPprojectMaker : public StMaker {
 private:
  // static Char_t  m_VersionCVS = "$Id: StppLPprojectMaker.h,v 1.1.1.2 2001/04/21 00:43:14 fisyak Exp $";

  TH1F *hst[16]; //!
  TH1F *hpol[3*MxSpinID]; //!

  void init_histo();

 protected:
 public: 
                  StppLPprojectMaker(const char *name="TLA");
   virtual       ~StppLPprojectMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual Int_t Finish(); 
   int *JspinID;
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

   virtual const char *GetCVS() const
     {static const char cvs[]="Tag $Name:  $ $Id: StppLPprojectMaker.h,v 1.1.1.2 2001/04/21 00:43:14 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}
   
   ClassDef(StppLPprojectMaker, 0)   //StAF chain virtual base class for Makers
};

#endif
