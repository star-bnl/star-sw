//*-- Author :    Valery Fine(fine@bnl.gov)   11/07/99  
//   
#ifndef STAR_StFtpcTrackFilter
#define STAR_StFtpcTrackFilter

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StFtpTrackFilter base class                                          //
//                                                                      //
//  Custom filter to filer Ftpc tracks only                             //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//
//////////////////////////////////////////////////////////////////////////

#include "StDefaultFilter.h"

class StFtpcTrackFilter : public StDefaultFilter  {

 protected:
    Int_t SubChannel(St_dst_track &track, Int_t index,Size_t &size, Style_t &style);
    virtual void  Distribution(St_dst_track *obj,TH1F &de);
 public:
     StFtpcTrackFilter():StDefaultFilter(){}
    ~StFtpcTrackFilter() {}
     ClassDef(StFtpcTrackFilter,0)
};
// $Log: StFtpcTrackFilter.h,v $
// Revision 1.2  2000/09/26 17:04:48  fine
// Two separate Distribution methods introduced
//
// Revision 1.1  2000/09/25 01:30:27  fine
// new StFtpcTrackFilter for Janet has been introdcued
//
#endif
