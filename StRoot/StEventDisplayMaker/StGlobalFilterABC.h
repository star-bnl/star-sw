//*-- Author :    Victor Perev(perev@bnl.gov)   11/09/04  
// $Id: StGlobalFilterABC.h,v 1.2 2004/10/07 19:41:23 perev Exp $ 
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
class TList;
class StEventDisplayMaker;

class TObjArray;
class StGlobalFilterABC : public TNamed  {
 public:
    StGlobalFilterABC(const char *name,const char *title);
    virtual ~StGlobalFilterABC();
    virtual void Filter(TObjArray *ebjs, int flag);
            void SetEvent(int nrun,int nev);
    virtual void NewEvent(int nrun,int nev);
            void SetActive(int act=1){fActive=act;}
            int  IsActive(){return fActive;}
            void SetMode(int mode){fMode=mode;}
            int  GetMode(){return fMode;}
            void SetMaker(StEventDisplayMaker *edm){fEDMaker=edm;}
static TList *GetList() {return fgGlobalList;}
protected:
 int fActive;
 int fMode;
 int fRun;		
 int fEvent;		
 int fNewEvent;
 StEventDisplayMaker *fEDMaker;
private:
static TList *fgGlobalList;

 		
    ClassDef(StGlobalFilterABC,0)
};
#endif
