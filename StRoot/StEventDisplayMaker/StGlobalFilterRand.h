//*-- Author :    Victor Perev(perev@bnl.gov)   11/09/04  
// $Id: StGlobalFilterRand.h,v 1.1 2004/10/17 03:37:19 perev Exp $ 
#ifndef STAR_StGlobalFilterRand
#define STAR_StGlobalFilterRand

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StGlobalFilterRand test class                                        //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#include "StGlobalFilterABC.h"
class StGlobalFilterRand : public StGlobalFilterABC {
 public:
    StGlobalFilterRand(int BadGood=3,int maxNum=1000000);
    virtual void Filter(TObjArray *eArr,int flag);
//  virtual void NewEvent(int nrun,int nev);
private:
    int fMaxNum;
    ClassDef(StGlobalFilterRand,0)
};
#endif
