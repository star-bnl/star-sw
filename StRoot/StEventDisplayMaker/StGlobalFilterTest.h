//*-- Author :    Victor Perev(perev@bnl.gov)   11/09/04  
// $Id: StGlobalFilterTest.h,v 1.2 2004/10/07 19:41:23 perev Exp $ 
#ifndef STAR_StGlobalFilterTest
#define STAR_StGlobalFilterTest

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StGlobalFilterTest test class                                        //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#include "StGlobalFilterABC.h"
class TH1F;
class TH2F;
class TCanvas;
class StGlobalFilterTest : public StGlobalFilterABC {
 public:
    StGlobalFilterTest();
    virtual void Filter(TObjArray *eArr,int flag);
    virtual void NewEvent(int nrun,int nev);
private:
TCanvas *fCanvas[2];
TH1F    *fHRes[5];
TH2F    *fPlot[1];
    ClassDef(StGlobalFilterTest,0)
};
#endif
