// $Id: StFtpcTrackFilter.cxx,v 1.2 2000/09/26 02:34:32 fine Exp $
#include "StFtpcTrackFilter.h"
#include "StDetectorId.h"

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
Int_t StFtpcTrackFilter::SubChannel(St_dst_track   &track, Int_t rowNumber,Size_t &size,Style_t &style)
{
  Int_t color = 0;
  long detector = track.GetTable(rowNumber)->det_id;
  if (detector == kFtpcWestId || detector == kFtpcEastId) {
     color = StDefaultFilter::SubChannel(track,rowNumber,size,style);
  }
  return color;
}
//_____________________________________________________________________________
// $Log: StFtpcTrackFilter.cxx,v $
// Revision 1.2  2000/09/26 02:34:32  fine
// debug print removed
//
// Revision 1.1  2000/09/25 01:30:26  fine
// new StFtpcTrackFilter for Janet has been introdcued
//

