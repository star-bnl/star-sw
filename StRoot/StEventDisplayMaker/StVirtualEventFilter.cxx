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
{return GetFlag();}
//_____________________________________________________________________________
Int_t StVirtualEventFilter::Filter(const StObjArray *hitCollection,Width_t &,Style_t &s)
{return GetFlag();}


