#include "StVirtualEventFilter.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StVirtualEventFilter virtual base class to create the variuous       //
// user-defined fikters for StEventDiplayMaker's objects                //
// $Id: StVirtualEventFilter.cxx,v 1.8 2000/01/12 18:07:22 fine Exp $
// This class defines the intreface to define a graphics attribute:     //
// like: color, size and style                                          //
// At the same time colr = 0 means the event supplied did not pass      //
// filter and should not be drawn at all                                //
//                                                                      //
// 1. StGlobalTrack  *globTrack                                         //
// 2. StObjArray     *hitCollection                                     //
// 3. St_TableSorter *tableObject,Int_t index                           //
// 4. StVertex       *vertexObject                                      //
// 5. St_Table       *tableObject, Int_t rowNumber                      //
//                                                                      //
// User should derive his own class and overload the "filter" methods   //
// with the signature those fit his/her current needs                   //
//                                                                      //
// Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StVirtualEventFilter)
//_____________________________________________________________________________
Int_t StVirtualEventFilter::GetFlag(){ return m_ActiveFlag;}

//_____________________________________________________________________________
Int_t StVirtualEventFilter::Channel(StGlobalTrack *,Size_t &,Style_t &)
{
   static Int_t colorTrack = kRed;
   colorTrack++;
   if (colorTrack > 20) colorTrack = kRed;
   return GetFlag() ? colorTrack : 0 ;
}
//_____________________________________________________________________________
Int_t StVirtualEventFilter::Channel(const StObjArray *,Size_t &,Style_t &)
{return GetFlag()? kYellow : 0 ;}

//_____________________________________________________________________________
Int_t StVirtualEventFilter::Channel(const St_TableSorter *,Int_t index,Size_t &,Style_t &)
{ return GetFlag()? (kGreen+index)%20 : 0 ;}
//_____________________________________________________________________________
Int_t StVirtualEventFilter::Channel(const StVertex *,Size_t &,Style_t &)
{ return GetFlag()? kBlue : 0 ; }

//_____________________________________________________________________________
Int_t StVirtualEventFilter::Channel(const St_Table *,Int_t rowNumber,Size_t &,Style_t &)
{ return GetFlag()? (kGreen+rowNumber)%20 : 0 ;}

//_____________________________________________________________________________
// $Log: StVirtualEventFilter.cxx,v $
// Revision 1.8  2000/01/12 18:07:22  fine
// cvs symbols have been added and copyright class introduced
//
//_____________________________________________________________________________


