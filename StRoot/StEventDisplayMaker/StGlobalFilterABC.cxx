// $Id: StGlobalFilterABC.cxx,v 1.2 2004/10/07 19:41:23 perev Exp $
#include "StGlobalFilterABC.h"
#include "TObjArray.h"
#include "TList.h"


//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  StGlobalFilterABC  class to filter the events from DST              //
//                                                                      //
//  It can manage the following tables:                                 //
//  St_dst_dedx , St_dst_point, St_dst_track                            //
//                                                                      //
//  Example provided by this filter :                                   //
//  begin_html <P ALIGN=CENTER> <IMG SRC="gif/EventDefaultFilter.gif" width=100%> </P> end_html   //
//                                                                      //
// - "doEvents" - defines the "bfc" should make NO reconstruction       //
//                and get the information from the begin_html <a href="http://www.rhic.bnl.gov/STAR/html/comp_l/ofl/dst_table_model.html">DST</a> end_html file directly    //
// - "y1h geant" - define the source of the detector geometry           //
// - "noevent"   - defines no StEvent output is required                //
//                                                                      //
// - small gray dots represent the hits                                 //
//   from begin_html <a href="dst_point_st.html">dst/point</a> end_html  table with no track associated                      //
//                                                                      //
// - "colored" cirles represent the hits associated with begin_html <a href="dst_track_st.html">dst/globtrk</a> end_html //
//   (The color is used to distinguish the hits of one track from others)//
//                                                                      //
// - "colored" lines represents the begin_html <a href="dst_track_st.html">dst/primtrk</a> end_html          //
//    The color index of the track represent the track dedx:            //
//   begin_html <font color=blue>Blue - small dedx (cool)</font> end_html                                          //
//   begin_html <font color=red>Red -  the larger dedx (hot)</font> end_html                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StGlobalFilterABC)
TList *StGlobalFilterABC::fgGlobalList=0;
//_____________________________________________________________________________
StGlobalFilterABC::StGlobalFilterABC(const char *name,const char *title):
                      TNamed(name,title),fRun(0),fEvent(0),fNewEvent(0)
{
   SetActive();
   SetMode(0);
   SetMaker(0);
   if (!fgGlobalList) fgGlobalList= new TList;
   fgGlobalList->Add(this);
}
//_____________________________________________________________________________
StGlobalFilterABC::~StGlobalFilterABC()
{
   fgGlobalList->Remove(this);
}


//_____________________________________________________________________________
void StGlobalFilterABC::Filter(TObjArray *ebjs, int flag)
{
  Warning("Filter","Not overloaded");
}
//_____________________________________________________________________________
void StGlobalFilterABC::SetEvent(int nrun,int nev)
{
   fNewEvent = 0;
   if (nrun == fRun && nev == fEvent) return;
   fNewEvent = 1;
   NewEvent( nrun,nev);	
   fRun=nrun; fEvent=nev;		
}		
//_____________________________________________________________________________
void StGlobalFilterABC::NewEvent(int ,int ){}
		
		
		
		
		
		
		
		

