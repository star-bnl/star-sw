//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
//   
//  
//
#ifndef STAR_St_TLA_EventFilter
#define STAR_St_TLA_EventFilter

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TLA_EventFilter base class                              //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include "StVirtualEventFilter.h"

class St_TLA_EventFilter : public StVirtualEventFilter  {
 public:
    St_TLA_EventFilter() {}
    virtual ~St_TLA_EventFilter(){;}
    virtual Int_t Filter(StGlobalTrack *globTrack,Width_t &size,Style_t &style);
    virtual Int_t Filter(const StObjArray *hitCollection,Width_t &size,Style_t &style);
    ClassDef(St_TLA_EventFilter,0)
};

#endif
