// $Id: StppLPprojectMaker.h,v 1.1.1.1 2001/01/31 14:00:07 balewski Exp $
// $Log: StppLPprojectMaker.h,v $
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
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

class StppLPprojectMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StppLPprojectMaker.h,v 1.1.1.1 2001/01/31 14:00:07 balewski Exp $";

  TH1F *hm[32]; //!
  TH1F *hst[16]; //!
  TH1F *hpol[4*3]; //!
  void init_histo();

 protected:
 public: 
                  StppLPprojectMaker(const char *name="TLA");
   virtual       ~StppLPprojectMaker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual Int_t Finish(); 
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StppLPprojectMaker.h,v 1.1.1.1 2001/01/31 14:00:07 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(StppLPprojectMaker, 1)   //StAF chain virtual base class for Makers
};

#endif
