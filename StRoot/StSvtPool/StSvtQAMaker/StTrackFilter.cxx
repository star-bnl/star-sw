#include "StTrackFilter.h"
#include "TTableSorter.h"
#include "TTableIter.h"
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_svt_hit_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_dst_point_Table.h"
#include "StCL.h"
#include "StHelix.hh"
#include "StThreeVector.hh"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTrackFilter virtual base class                                     //
//                                                                      //
//                                                                      //
//  Submit any problem with this code via begin_html <A HREF="http://www.star.bnl.gov/STARAFS/comp/sofi/bugs/send-pr.html"><B><I>"STAR Problem Report Form"</I></B></A> end_html
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(StTrackFilter)

//_____________________________________________________________________________
Int_t StTrackFilter::SetTrack_P(Int_t *track_p,Int_t n)
{
  m_LTrackP = n;
  StCL::ucopy(track_p,m_Track_P, m_LTrackP);
  return 0; 
}
//_____________________________________________________________________________
Int_t StTrackFilter::SetTptID(Int_t *track_id,Int_t n)
{
   m_Ltpt_track = n;
   StCL::ucopy(track_id,m_tpt_track_id, m_Ltpt_track); 
   return 0;
}
//_____________________________________________________________________________
Int_t StTrackFilter::SetId_globtrk(Int_t *id_globtrk,Int_t n)
{
   m_Lid_globtrk = n;
   StCL::ucopy(id_globtrk,m_id_globtrk, m_Lid_globtrk); 
   return 0;
}
//_____________________________________________________________________________
Int_t StTrackFilter::SetGe_pid(Int_t pid){ m_Ge_pid  = pid; return m_Ge_pid;}

//_____________________________________________________________________________
Int_t StTrackFilter::Reset(Int_t reset)
{
 m_G2t_track = 0; m_G2t_vertex = 0;
 SafeDelete(m_NextG2tTrack);
 return reset; 
}
//_____________________________________________________________________________
Int_t StTrackFilter::SubChannel(St_tcl_tphit &hit, Int_t rowNumber,Size_t &size, Style_t &style)
{
  size = 0.5;
  return kGreen;
  return hit[rowNumber].id_globtrk;
  if ( m_Lid_globtrk)  { 
    for (int i =0; i <m_Lid_globtrk; i++) {
      if (hit[rowNumber].id_globtrk !=  m_id_globtrk[i]) continue;
      printf("hit %d %ld \n", rowNumber, hit[rowNumber].id_globtrk); 
      style = 6;
      size = 2;
      return kGreen+rowNumber%20;       
    }
  }
  return 0;
}
//_____________________________________________________________________________
Int_t StTrackFilter::SubChannel(St_dst_point &hit, Int_t rowNumber,Size_t &size, Style_t &style)
{
  if ( m_Lid_globtrk)  { 
    for (int i =0; i <m_Lid_globtrk; i++) {
      if (hit[rowNumber].id_track !=  m_id_globtrk[i]) continue;
      //printf("hit %d %ld \n", rowNumber, hit[rowNumber].id_track); 
      style = 6;
      size = 2;
      return kGreen;       
    }
  }
  return 0;
}
//_____________________________________________________________________________
Int_t StTrackFilter::SubChannel(St_g2t_svt_hit &hit, Int_t rowNumber,Size_t &size,Style_t &style)
{
  if ( m_LTrackP)  { 
    for (int i =0; i <m_LTrackP; i++) {
      if (hit[rowNumber].track_p !=  m_Track_P[i]) continue;
      printf("hit %d %ld \n", rowNumber, hit[rowNumber].track_p); 
      style = 4;
      size = 1;
      return kBlue; 
    }
  }
  return 0;
}
//_____________________________________________________________________________
Int_t StTrackFilter::SubChannel(St_g2t_tpc_hit &hit, Int_t rowNumber,Size_t &size,Style_t &style)
{
 Int_t color = 0;
 style = 4;
 size  = 1;
 long track_p = hit[rowNumber].track_p; //  Id of parent track 
 if (m_G2t_track) {

   TTableIter &nextTrack = *m_NextG2tTrack;
   St_g2t_track &track     = *((St_g2t_track*)m_G2t_track->GetTable());
   Int_t indxColor         =  nextTrack[0];
   
   if (indxColor >= 0 ) // Check whether this table does contain any "ge_pid" we are looking for
   {
     Int_t indxRow           = -1;

     color  = track[indxColor].ge_pid+1;

     while ( (indxRow = nextTrack()) >= 0 ) {
       if (track_p != track[indxRow].id) continue;

       if (m_G2t_vertex && track[indxRow].stop_vertex_p) {
          TTableIter nextVertex(m_G2t_vertex,track[indxRow].stop_vertex_p);

          if (nextVertex.GetNRows() > 0) {
             St_g2t_vertex &vertex = *((St_g2t_vertex*)m_G2t_vertex->GetTable());
             while ( (indxRow = nextVertex() ) >=0 ) {
               Float_t *x = &(vertex[indxRow].ge_x[0]);
               Float_t dist = x[0]*x[0]+x[1]*x[1];
               Bool_t condition = dist > 133*133 && dist < 179*179; 

                if ((vertex[indxRow].ge_proc == 5) && condition ) 
                        return color%8+1;                             
             }
 	   }
         }

         return 0;      
       }
    }
 }
  if ( m_LTrackP)  { 
    for (int i =0; i <m_LTrackP; i++) {
      if ( track_p !=  m_Track_P[i])  continue;
      printf("hit %d %ld  \n", rowNumber, track_p); 
      return kBlue; 
    }
  }
  return 0;
}

//_____________________________________________________________________________
Int_t StTrackFilter::Channel(const TTableSorter *tableObject,Int_t index,Size_t &size,Style_t &style)
{
  //
  // This is an example how one can separate tableObject's by the table "name"
  // rather by the table "type" as the next "Channel" does
  //
 Int_t rowNumber = tableObject->GetIndex(UInt_t(index));
 
 Color_t color = -1;
 TString mStr = tableObject->GetTable()->GetName();

 if( mStr == "g2t_track" && !m_G2t_track) {
   if (m_NextG2tTrack) delete m_NextG2tTrack;
   m_NextG2tTrack = new TTableIter(tableObject,m_Ge_pid);
   m_G2t_track = tableObject;
 }
 else if( mStr == "g2t_vertex" && !m_G2t_vertex) {
   m_G2t_vertex   = tableObject;   
 }
 else if( mStr == "g2t_tpc_hit") {
   St_g2t_tpc_hit &hit = *((St_g2t_tpc_hit *)tableObject->GetTable());    
   color = SubChannel(hit, rowNumber, size, style);
 }
 else if( mStr == "g2t_svt_hit") {
   St_g2t_svt_hit &hit = *((St_g2t_svt_hit *)tableObject->GetTable());     
   color = SubChannel(hit,rowNumber,size,style);
 }
 else if( mStr == "tphit" ) {
   St_tcl_tphit &hit = *((St_tcl_tphit *)tableObject->GetTable());     
   color = SubChannel(hit,rowNumber,size,style);
 }
 else if( mStr == "point" ) {
   St_dst_point &hit = *((St_dst_point *)tableObject->GetTable());     
   color = SubChannel(hit,rowNumber,size,style);
 }
 return color; 
}

//_____________________________________________________________________________
Int_t StTrackFilter::Channel(const TTable *tableObject,Int_t rowNumber,Size_t &size,Style_t &style)
{
  // Introduce in here your own rule to select a particular row of the given tableObjectvertex
  //
  // Input:
  //   St_Table *tableObject - pointer
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
  TString mStr = tableObject->GetName();

  if (((mGlobal) && (mStr == "globtrk")) ||
      ((mPrimary) && (mStr == "primtrk"))) {
    St_dst_track &track = *((St_dst_track *)tableObject); 
    return SubChannel(track, rowNumber, size, style);
  } 
  else
    return 0;
}
//_____________________________________________________________________________
Int_t StTrackFilter::SubChannel(St_dst_track   &track, Int_t rowNumber,Size_t &size,Style_t &style)
{
  // SubChannel to provide a selections for St_dst_track tracks.
  float s=0;

  if (mTracksHitSvt)
    s = GetLength(track,rowNumber);

  style = 6;
  size = 2;
  if (s || mAllTracks) {
    //track[rowNumber].length = s;	
    if (mDrawHits) {
      m_id_globtrk[m_Lid_globtrk] = track[rowNumber].id;
      m_Lid_globtrk++;
    }
	
    if (mDrawTracks)
      return kBlue;
    else
      return 0;
  }
  else
    return 0;
}

//_____________________________________________________________________________
double StTrackFilter::GetLength(St_dst_track & glb, Int_t i)
{
  int wafer=0, hybrid=0;
  double curv, dip, phase, s0, x0, y0, z0, s, x, z;
  int h;
  StThreeVector<double> r0(0.,0.,0.);
  StThreeVector<double> r(0.,10.4,0.);
  StThreeVector<double> n(0.,1.,0.);
  const float pi2 = 3.1415926/2.;
  const float rad = pi2/90.;

  x0 = glb[i].r0*cos(glb[i].phi0*rad);
  y0 = glb[i].r0*sin(glb[i].phi0*rad);
  z0 = glb[i].z0;

  curv = glb[i].curvature;
  dip = atan(glb[i].tanl);
  h = glb[i].icharge > 0 ? -1 : 1;;
  phase = glb[i].psi*rad-h*pi2;
  StThreeVector<double> origin(x0,y0,z0);

  StHelix helix(curv,dip,phase,origin,h);

  s0 = helix.pathLength(r0,n);
  s = helix.pathLength(r,n);

  //if ((s < 0) || (s > 1000)) return 0;
  if ((s < s0) || (s > 1000)) return 0;

  x = helix.x(s);
  z = helix.z(s);

  if ((x > 0.) && (x < 3.))
    hybrid = 2;
  else if ((x < 0.) && (x > -3.))
    hybrid = 1;

  if ((z > -22.05) && (z < -15.75))
    wafer = 1;
  else if ((z > -15.75) && (z < -9.45))
    wafer = 2;
  else if ((z > -9.45) && (z < -3.15)) 
    wafer = 3;
  else if ((z > -3.15) && (z < 3.15))
    wafer = 4;
  else if ((z > 3.15) && (z < 9.45))
    wafer = 5;
  else if ((z > 9.45) && (z < 15.75))
    wafer = 6;
  else if ((z > 15.75) && (z < 22.05))
    wafer = 7;

  //if ((fabs(x)<3.) && fabs(z)<22.05) {
  if ((wafer == waferID) && (hybrid == hybridID)) {
    cout << "x = " << x << ", z = " << z << endl;
    return s;
  }
  else
    return 0;
}
