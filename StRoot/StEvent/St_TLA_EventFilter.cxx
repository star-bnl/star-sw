#include "St_TLA_EventFilter.h"
#include "StGlobalTrack.h"
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_TLA_EventFilter virtual base class                              //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(St_TLA_EventFilter)
//_____________________________________________________________________________
Int_t St_TLA_EventFilter::Filter(StGlobalTrack *globTrack,Width_t &size,Style_t &style)
{ 
  // Introduce in here your own rule to select globTrack
  // Return value: >0  the color index this track should be drawn with
  //              = 0  this track will be not drawn
  //         size (option) - the width of the lines used to draw the selected track
  //         style(option) - the line style to be used to draw the selcted track
  Int_t color = StVirtualEventFilter::Filter(globTrack,size,style);
  if (GetFlag() == 2 && globTrack->length() < 250) color = 0; 
  return color; 

}
//_____________________________________________________________________________
Int_t St_TLA_EventFilter::Filter(const StObjArray *hitCollection,Width_t &size,Style_t &style)
{
  // Introduce in here your own rule to select hitCollection
  // Return value: >0 the color index the hits from the entire collection will be drawn
  //              = 0  this collection will be not drawn
  //         size (option) - the marker size to be used to draw each hit from collection
  //         style(option) - the marker style to be used to draw each hit from collection

 return StVirtualEventFilter::Filter(hitCollection,size,style); 
}



