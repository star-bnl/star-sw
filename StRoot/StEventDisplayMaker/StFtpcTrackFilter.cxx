// $Id: StFtpcTrackFilter.cxx,v 1.4 2000/11/17 22:26:47 fine Exp $
#include "StFtpcTrackFilter.h"
// #include "StDetectorId.h"
#include "TH1.h"
#include "TTableSorter.h"
#include "St_dst_trackC.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_point_Table.h"
#include "StDstPointChair.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//   StFtpcTrackFilter is a custom version of StDefaultFilter  class    //
//   It filters "FTPC" tracks only
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcTrackFilter)

//_____________________________________________________________________________
void StFtpcTrackFilter::Distribution(St_dst_track *trackT,TH1F &de) 
{
   // Calculate momentum distribution for FTPC only
   St_dst_trackC c(trackT);
   dst_track_st *track = trackT->begin();
   Int_t cEnd = c.GetNRows();
   Int_t i = 0;
   for (i=0;i<cEnd;i++,track++) {
      long detector = track->det_id;
      if (detector == kFtpcWestId || detector == kFtpcEastId )
            de.Fill(c.AbsMoment(i));
   }
}
//_____________________________________________________________________________
Int_t StFtpcTrackFilter::SubChannel(const TTableSorter *tableObject, Int_t index,Size_t &size, Style_t &style)
{
  Int_t color = 0;
  St_dst_point *hit = (St_dst_point *)tableObject->GetTable();
  assert(hit);
  StDstPointChair thisHit(hit);
  Int_t idx = tableObject->GetIndex(index);
  UShort_t detId = thisHit.DetectorId(idx);
  if (detId == kFtpcWestId || detId == kFtpcEastId) { 
     dst_point_st &point = *hit->GetTable(idx);
     // Set "small" marker for the "noise" points
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
  }
  return color;
}
//_____________________________________________________________________________
Int_t StFtpcTrackFilter::SubChannel(St_dst_track   &track, Int_t rowNumber,Size_t &size,Style_t &style)
{
  Int_t color = 0;  
  long detector = track.GetTable(rowNumber)->det_id;
  if (detector == kFtpcWestId || detector == kFtpcEastId) {
     if (!GetColorAxis() && rowNumber != 0) CreatePalette(&track);
     color = StDefaultFilter::SubChannel(track,rowNumber,size,style);
  }
  return color;
}
//_____________________________________________________________________________
// $Log: StFtpcTrackFilter.cxx,v $
// Revision 1.4  2000/11/17 22:26:47  fine
// packing factors for Ftpc adjusted
//
// Revision 1.3  2000/09/26 17:04:48  fine
// Two separate Distribution methods introduced
//
// Revision 1.2  2000/09/26 02:34:32  fine
// debug print removed
//
// Revision 1.1  2000/09/25 01:30:26  fine
// new StFtpcTrackFilter for Janet has been introdcued
//

