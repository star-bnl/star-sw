#include "StVirtualEventFilter.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVirtualEventFilter virtual base class                              //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StVirtualEventFilter)
//_____________________________________________________________________________
Int_t StVirtualEventFilter::GetFlag(){ return m_ActiveFlag;}

//_____________________________________________________________________________
Int_t StVirtualEventFilter::Filter(StGlobalTrack *,Width_t &,Style_t &)
{
   static Int_t colorTrack = kRed;
   colorTrack++;
   if (colorTrack > 20) colorTrack = kRed;
   return GetFlag() ? colorTrack : 0 ;
}
//_____________________________________________________________________________
Int_t StVirtualEventFilter::Filter(const StObjArray *,Width_t &,Style_t &)
{return GetFlag()? kYellow : 0 ;}

//_____________________________________________________________________________
Int_t StVirtualEventFilter::Filter(const St_Table *,Int_t index,Width_t &,Style_t &)
{ return GetFlag()? (kGreen+index)%20 : 0 ;}
//_____________________________________________________________________________
Int_t StVirtualEventFilter::Filter(const StVertex *,Width_t &,Style_t &)
{ return GetFlag()? kBlue : 0 ; }

