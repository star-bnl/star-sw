// $Id: StDefaultFilter.cxx,v 1.8 2000/09/26 17:04:46 fine Exp $
#include "iostream.h"
#include "TH1.h"
#include "StDefaultFilter.h"
#include "TTableSorter.h"
#include "TTableIter.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_dedx_Table.h"
#include "tables/St_dst_point_Table.h"
#include "St_dst_trackC.h"
#include "StCL.h"
#include "TColoredAxis.h"

#include "TStyle.h"


//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  StDefaultFilter  class to filter the events from DST                //
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

ClassImp(StDefaultFilter)
//_____________________________________________________________________________
StDefaultFilter::StDefaultFilter()
{
   mColorAxis = 0;
   Reset();
}
//_____________________________________________________________________________
Int_t StDefaultFilter::SetTrack_Stylez(Int_t lowcolor,float lowenergy,Int_t highcolor,float highenergy,Size_t size,Style_t style,
Int_t CutTracksOutOfRange, Int_t KindOfColoring)
{
 STFLowColor   = lowcolor; 
 STFHighColor  = highcolor;
 STFLowEnergy  = lowenergy;
 STFHighEnergy = highenergy;
// STFSize       = size;
 STFStyle      = style;
 STFCutTracksOutOfRange = CutTracksOutOfRange ;
 STFKindOfColoring      = KindOfColoring ;
 return 0 ;
}

//_____________________________________________________________________________
Int_t StDefaultFilter::Reset(Int_t reset)
{
  mDedx = 0;
 
  STFLowColor  = 51; 
  STFHighColor = 100;
  STFLowColor  = 0; 
  STFHighColor = 51;

  STFLowEnergy = 0;
  STFHighEnergy = 0.000006;
  STFSize  = 0;
  STFStyle = 0; 
  STFColorEnergy = 0;
  STFColor = 0;
  STFCutTracksOutOfRange = 0;
  STFKindOfColoring = 0;
  mlookFactor = 1;
  if (mColorAxis) { delete mColorAxis; mColorAxis = 0;}
  return reset; 
}
//________________________________________________________________________________
Int_t StDefaultFilter::MakeColor(Double_t energy)
{
 STFColor = 0;
 if ( energy>STFHighEnergy )  
   STFColor = STFHighColor;
 else if ( energy<STFLowEnergy ) 
   STFColor = STFLowColor;
 else  {
       STFColor = STFLowColor
                  + int((STFHighColor-STFLowColor)
                   *mDeLookUp[int((energy - STFLowEnergy)*mlookFactor)]);
 }
 return STFColor;
}

//_____________________________________________________________________________
Int_t StDefaultFilter::Channel(const TTableSorter* tableObject,Int_t index,Size_t &size,Style_t &style)
{
  //
  // This is an example how one can separate tableObject's by the table "name"
  // rather by the table "type" as the next "Channel" does
  //
  // Int_t rowNumber = tableObject->GetIndex(UInt_t(index));
 
  Color_t color = -1;
  TString mStr = tableObject->GetTable()->GetName();
  if(mStr == "dst_dedx" && !mDedx)
  {
    mDedx = tableObject;
    CreatePalette(tableObject->GetTable());
  } else if (mStr == "point"){
     color = SubChannel(tableObject, index, size, style);
  } else {
     color = StVirtualEventFilter::Channel(tableObject,index,size,style);   
  }
  return color; 
}
//_____________________________________________________________________________
Int_t StDefaultFilter::Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style)
{
  // Introduce in here your own rule to select a particular row of the given tableObjectvertex
  //
  // Input:
  //   TTable *tableObject - pointer
  //   Int_t rowNumber       - the current row number
  // 
  // Return value: > 0 the color index this vertex will be drawn
  //               = 0 this vertex will be not drawn
  //               < 0 no track will be drawn fro  now untill StVirtualFilter::Reset called
  // Output:
  //         size (option) - the marker size to be used to draw this vertex
  //         style(option) - the marker style to be used to draw this vertex
  // Note: One may not assign any value for the output parameters size and style
  //       The "default" values will be used instead.
  //
  // This is an example how one can separate tableObject's by the table "type"
  // rather by the table "name" as the previous "Channel" does
  //

  TString mStr = tableObject->GetType();
  if( mStr == "dst_track_st" ) {
     St_dst_track &track = *((St_dst_track *)tableObject); 
     return SubChannel(track,rowNumber,size,style);
  }
  return StVirtualEventFilter::Channel(tableObject,rowNumber,size,style);
}
//_____________________________________________________________________________
Int_t StDefaultFilter::SubChannel(const TTableSorter *tableObject, Int_t index,Size_t &size, Style_t &style)
{
  Int_t color = 0;
  St_dst_point *hit = (St_dst_point *)tableObject->GetTable();
  assert(hit);
  dst_point_st &point = *hit->GetTable(tableObject->GetIndex(index));
  // Set "small" marker for the "noice" points
  if (point.id_track == 0) {
     // small gray points
     style = 1;  
     color = 18;
  } else {
    // "regular" circles
    style = 4;
    size  = 0.35;
    color = StVirtualEventFilter::Channel(tableObject,index,size,style);
  }
  return color;
}

//_____________________________________________________________________________
Int_t StDefaultFilter::SubChannel(St_dst_track   &track, Int_t rowNumber,Size_t &size,Style_t &style)
{
  Int_t color = -1;
  // SubChannel to provide a selections for St_dst_track tracks.
  Double_t trackColor;
  if (mDedx) {
    const St_dst_dedx &t = *(St_dst_dedx *)mDedx->GetTable();
    trackColor = t.GetTable((*mDedx)[rowNumber-1])->dedx[0];   
  } else {    
    if (rowNumber == 0)  CreatePalette(&track);
    St_dst_trackC chair(&track);
    trackColor = chair.AbsMoment(rowNumber);
  }
  color  =  gStyle->GetColorPalette(MakeColor(trackColor));
  return color;
}
//_____________________________________________________________________________
Int_t StDefaultFilter::CreatePalette(TTable *obj)
{
   if (!obj) return 0;
   // Create lookup table
   Int_t lookUpSize = sizeof(mDeLookUp);
   mNbins = lookUpSize/sizeof(mDeLookUp[0]);

   Float_t low, high;
   if (mColorAxis) delete mColorAxis; 
   TString str = obj->GetType();
   if (mDedx) {
     low  = STFLowEnergy  = 0;
     high = STFHighEnergy = 0.000005;
   } else {
     low  = STFLowEnergy  = 0.1;
     high = STFHighEnergy = 5;
   }
//   mColorAxis = new TColoredAxis(0.9,-0.95,0.9,0.95,low,high);
   mColorAxis = new TColoredAxis(0.9,-0.95,0.9,0.95,low,high,mDeLookUp,mNbins);
   TH1F de("__de__","dedx",mNbins,low,high);

      if (mDedx) 
        Distribution((St_dst_dedx *)obj,de);
      else
        Distribution((St_dst_track *)obj,de);

   Double_t  s = de.ComputeIntegral();
   Int_t hSize = mNbins;
   if (s > 0) {
     Double_t *f = de.GetIntegral();
     for (hSize = 0; hSize < mNbins; hSize++) {
       mDeLookUp[hSize] = f[hSize]/s;
       if ( mDeLookUp[hSize] > 0.995) break; // Look for the rightmost non-zero bin
     }
   } else { 
     TCL::vzero(mDeLookUp,hSize+1);
   }
   // Corect edges
   high = STFHighEnergy = de.GetBinCenter(hSize);
   mColorAxis = new TColoredAxis(0.9,-0.95,0.9,0.95,low,high,mDeLookUp,hSize+1);
   if (mDedx) 
     mColorAxis->SetTitle("tracks color = F(de/dx)");
   else
     mColorAxis->SetTitle("tracks color = F(momentum)");
    
   mlookFactor = 1./de.GetBinWidth(1);
   mColorAxis->SetTitleSize(0.025);
   mColorAxis->SetTextColor(14);
   mColorAxis->SetLabelColor(14);
   mColorAxis->SetLabelSize(0.02);
   mColorAxis->SetLineColor(14);
   mColorAxis->Draw();
   return 0;
}

//_____________________________________________________________________________
void StDefaultFilter::Distribution(St_dst_dedx *dedxT,TH1F &de) 
{
 // Calculate De/Dx distribution
  St_dst_dedx *t    = dedxT;
  dst_dedx_st *dedx = 0;
  dst_dedx_st *dEnd = t->end();
  for (dedx = t->begin();dedx !=dEnd; dedx++)
                   de.Fill(dedx->dedx[0]);
}
//_____________________________________________________________________________
void StDefaultFilter::Distribution(St_dst_track *track,TH1F &de) 
{
 // Calculate momentum distribution
  St_dst_trackC c(track);
  Int_t cEnd = c.GetNRows();
  Int_t i = 0;
  for (i=0;i<cEnd;i++) de.Fill(c.AbsMoment(i));
}
//_____________________________________________________________________________
// $Log: StDefaultFilter.cxx,v $
// Revision 1.8  2000/09/26 17:04:46  fine
// Two separate Distribution methods introduced
//
// Revision 1.7  2000/09/06 21:57:52  fine
// Dynamic color axis
//
// Revision 1.6  2000/09/01 22:39:00  fine
// minor bug fixex (Sun complained)
//
// Revision 1.5  2000/08/30 17:22:00  fine
// Clean up
//
// Revision 1.4  2000/08/29 04:39:14  fine
// RemoveName method introduced
//
// Revision 1.3  2000/08/27 19:30:48  fine
// Bug fix: SubChannel(dst_point) fixed
//
// Revision 1.2  2000/08/27 16:55:07  fine
// Title with Run event number etc
//
// Revision 1.1  2000/08/26 03:14:42  fine
// New default filter from M.Panebratcev has been introduced
//
