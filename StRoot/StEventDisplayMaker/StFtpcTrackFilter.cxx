// $Id: StFtpcTrackFilter.cxx,v 1.3 2000/09/26 17:04:48 fine Exp $
#include "StFtpcTrackFilter.h"
// #include "StDetectorId.h"
#include "TH1.h"
#include "St_dst_trackC.h"
#include "tables/St_dst_track_Table.h"

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
// Revision 1.3  2000/09/26 17:04:48  fine
// Two separate Distribution methods introduced
//
// Revision 1.2  2000/09/26 02:34:32  fine
// debug print removed
//
// Revision 1.1  2000/09/25 01:30:26  fine
// new StFtpcTrackFilter for Janet has been introdcued
//

