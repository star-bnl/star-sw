// $Id: StHistCollectorMaker.h,v 2.4 2014/08/06 11:42:52 jeromel Exp $

#ifndef STAR_StHistCollectorMaker
#define STAR_StHistCollectorMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StHistCollectorMaker is to collect the histBranch staff from the several //
//                    files                                             //
//                                                                      //
// use $STAR/StRoot/macro/analysis/doHists.C macro to see hot it works  //
//  Submit any problem with this code via begin_html <A HREF="http://www.rhic.bnl.gov/STAR/html/comp_l/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class StHistCollectorMaker : public StMaker {
 private:
// static Char_t  m_VersionCVS = "$Id: StHistCollectorMaker.h,v 2.4 2014/08/06 11:42:52 jeromel Exp $";
 
 protected:
  TDataSet *fMergedSet;
 public: 
                  StHistCollectorMaker(const char *name="TLA");
   virtual       ~StHistCollectorMaker();
   TDataSet      *AddHists();
   virtual Int_t Init();
   virtual Int_t  Make();
// virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
// virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
   void  UpdateHists(TObjectSet *oldSet,TObjectSet *newSet);


   virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHistCollectorMaker.h,v 2.4 2014/08/06 11:42:52 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

   ClassDef(StHistCollectorMaker,0)   //StAF chain virtual base class for Makers
};

// $Log: StHistCollectorMaker.h,v $
// Revision 2.4  2014/08/06 11:42:52  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 2.3  2003/09/10 19:47:02  perev
// ansi corrs
//
// Revision 2.2  2000/11/30 19:37:27  fine
// Reference to doHists.C macro has been added
//
// Revision 2.1  2000/11/30 19:35:14  fine
// New analysis utility to collect all histogram from all histBranh production branches
//

#endif

