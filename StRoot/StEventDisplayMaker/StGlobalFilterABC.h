//*-- Author :    Victor Perev(perev@bnl.gov)   11/09/04  
// $Id: StGlobalFilterABC.h,v 1.1 2004/09/28 03:55:23 perev Exp $ 
#ifndef STAR_StGlobalFilterABC
#define STAR_StGlobalFilterABC

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StGlobalFilterABC base class                                      //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////
#include "TNamed.h"
#include "StEventHelper.h"

class TObjArray;
class StGlobalFilterABC : public TNamed  {
 public:
    StGlobalFilterABC(const char *name,const char *title);
    virtual ~StGlobalFilterABC() {;}
    virtual void Filter(TObjArray *ebjs, int flag);
            void SetEvent(int nrun,int nev);
    virtual void NewEvent(int nrun,int nev);
protected:
 int fRun;		
 int fEvent;		
 int fNewEvent;
 		
    ClassDef(StGlobalFilterABC,0)
};
#endif
