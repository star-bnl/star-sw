#include "StSectorHitFilter.h"
#include "TTableSorter.h"
#include "TTableIter.h"

#include "StTclHitChair.h"

#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_dst_track_Table.h"
#include "StCL.h"


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSectorHitFilter virtual base class                                 //
//                                                                      //
// Filter assumes one wants to present things per /sector/padrow        //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StSectorHitFilter)

//_____________________________________________________________________________
Int_t StSectorHitFilter::SetSecRow(Int_t *sectorRows,Int_t n)
{
   m_nRow    = n;
   StCL::ucopy(sectorRows,m_row, m_nRow); 
   printf( " # %d; ", m_nRow);
   if (m_nRow) printf( " Row %d", m_row[0]);
   printf("\n");
   return 0;
}
//_____________________________________________________________________________
Int_t StSectorHitFilter::Reset(Int_t reset)
{
 printf(" bad hits = %d, # track %d\n",m_badCounter,m_trackId.size());
 m_Primtrk    = 0;
 m_badCounter = 0;
 m_trackId.clear();
 return reset; 
}
//_____________________________________________________________________________
Int_t StSectorHitFilter::HitSubChannel(const TTableSorter *tableObject, Int_t index,Size_t &size, Style_t &style)
{
  Color_t color = -1;
  TString mStr = tableObject->GetTable()->GetName();
  St_tcl_tphit *hit = (St_tcl_tphit *)tableObject->GetTable();
  if (hit) {
    // Take sector / padrow 
    StTclHitChair thisHit(hit);
    Int_t rowNumber = tableObject->GetIndex(UInt_t(index));
    if ( m_nRow)  { 
      UInt_t indx = UInt_t(index);
      for (int i =0; i < m_nRow; i++) {
        if (thisHit[rowNumber].row !=  m_row[i]) continue;
        // remember the track if of this hit
        while (thisHit[rowNumber].row == m_row[i] && rowNumber >= 0 ) { 
//           long id = thisHit.TrackId(rowNumber);  // tpt track id
           long id = thisHit[rowNumber].id_globtrk;
           if (id) 
              m_trackId.insert(id);
           else 
              m_badCounter++;
           indx++; 
           rowNumber = tableObject->GetIndex(indx);
        }
        printf(" %d tracks have been found\n", m_trackId.size());
          style = 6;
//        size = 3.0;
        color = 51+rowNumber%50;        
        return color;
      }
    } else {printf(" No rows defined !!!\n"); }
    
  }
  else 
      Error("HitSubChannel","No hit table supplied");
  return 0;
}
//_____________________________________________________________________________
Int_t StSectorHitFilter::Channel(const TTableSorter *tableObject,Int_t index,Size_t &size,Style_t &style)
{
  //
  // This is an example how one can separate tableObject's by the table "name"
  // rather by the table "type" as the next "Channel" does
  //
  Color_t color = -1;
  TString mStr = tableObject->GetTable()->GetName();

  if( mStr == "tphit" ) {
    color = HitSubChannel(tableObject,index,size,style);
  } else if( !m_Primtrk && mStr == "primtrk") {
    m_Primtrk   = tableObject;   
  }
  return color; 
}

//_____________________________________________________________________________
Int_t StSectorHitFilter::Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style)
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
  static int colorIndex = 0;
  TString mStr = tableObject->GetType();

  if( mStr == "dst_track_st" ) {
    St_dst_track &track = *((St_dst_track *)tableObject); 
    return SubChannel(track, rowNumber, size, style);
  } else {  
    St_tpt_track &track = *((St_tpt_track *)tableObject); 
    if (!m_trackId.empty() )  {    
      for (  set<long>::iterator i = m_trackId.begin(); i != m_trackId.end(); i++ ) {
         if (track[rowNumber].id != *i) continue;
         // style = 6;  //custom line style
         // size = 1.6; //custom line width 
         colorIndex++;
         if (colorIndex > 20) colorIndex = 0;
         return kBlue+colorIndex;
     }
   }
 }
 return 0; 
}
//_____________________________________________________________________________
Int_t StSectorHitFilter::SubChannel(St_dst_track   &track, Int_t rowNumber,Size_t &size,Style_t &style)
{ 
  // SubChannel to provide a selections for St_dst_track tracks.
  // It selects all track with id provided via "set m_trackId"
  // and check it agaist of "primtrk" table if available.

  // It set kBlue color for all tracks not found in primtrk table
  // and set kYellow otherwise.
  
  int colorIndex = 0;
  if (!m_trackId.empty() )  {    
     for (  set<long>::iterator i = m_trackId.begin(); i != m_trackId.end(); i++ ) {
        if (track[rowNumber].id != *i) continue;
        colorIndex = kYellow;
        if (m_Primtrk &&
            (*m_Primtrk)[track[rowNumber].id] < 0) colorIndex = kBlue;
        // style = 6;
        // size = 1.6;
        return colorIndex;     
     }
   }
   return colorIndex;
}
