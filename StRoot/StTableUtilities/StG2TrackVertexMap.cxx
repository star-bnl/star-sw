#include <assert.h>
#include "StG2TrackVertexMap.h"
#include "TVector3.h"
#include "StMessMgr.h"

StG2TrackVertexMap *StG2TrackVertexMap::fgInstance = 0;
//________________________________________________________________________________
StG2TrackVertexMap *StG2TrackVertexMap::instance(St_g2t_track *track, St_g2t_vertex *vertex) {
  if (! fgInstance) {fgInstance = new StG2TrackVertexMap(track,vertex); }
  else if (track && vertex) {
    fgInstance->Reset(track,vertex);
  }
  return fgInstance;
}
//________________________________________________________________________________
void StG2TrackVertexMap::Reset(St_g2t_track *track, St_g2t_vertex *vertex) {
  // Clear the maps
  fTrack2Vertex.clear();
  fVertex2ParentTrack.clear();
  // Return if there is no track or vertex table
  if (! track ||  ! vertex) return;
  // Number of tracks and vertices
  int Nt = track->GetNRows();
  int NV = vertex->GetNRows();
  g2t_track_st *t = track->GetTable();
  g2t_vertex_st *v = vertex->GetTable();
  static TVector3 Vx1;
  static TVector3 Vx2;
  // Loop over all tracks
 next:
  for (int i = 0; i < Nt; i++) {
    int IdT = t[i].id - 1;                 assert(IdT >= 0 && IdT < Nt);
    int IdV = t[i].start_vertex_p - 1;     assert(IdV >= 0 && IdV < NV);    
    Vx1.SetXYZ( v[IdV].ge_x[0], v[IdV].ge_x[1], v[IdV].ge_x[2] );
    float tof1 = v[IdV].ge_tof;
    int IdP = v[IdV].parent_p - 1;         
    if (IdP >= 0 && IdP < Nt) {
      int IdVxP = t[IdP].start_vertex_p - 1; assert(IdVxP >= 0 && IdVxP < NV);
      Vx2.SetXYZ( v[IdVxP].ge_x[0], v[IdVxP].ge_x[1], v[IdVxP].ge_x[2] );
      Vx2 -= Vx1;
      // Merge short lived decays with any (grand) parent vertex 
      while ((Vx2.Mag() < 10e-4 && TMath::Abs(tof1-v[IdVxP].ge_tof) < 1e-9)) {
	IdV = IdVxP;
	IdP = v[IdV].parent_p - 1;          if ( IdP < 0      ) break;
	IdVxP = t[IdP].start_vertex_p - 1;  if ( IdVxP < 0    ) break;
                                            if ( IdV == IdVxP ) break; // poorly defined intermediate vertices
	Vx2.SetXYZ( v[IdVxP].ge_x[0], v[IdVxP].ge_x[1], v[IdVxP].ge_x[2] );
	Vx2 -= Vx1;
      }
    }
    fTrack2Vertex[IdT+1] = IdV+1;
  }
  for (int i = 0; i < NV; i++) {
    fVertex2ParentTrack[v[i].id] = v[i].parent_p;
  }
}
