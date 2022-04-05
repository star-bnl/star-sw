#include <assert.h>
#include "StG2TrackVertexMap.h"
#include "TVector3.h"
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
  fTrack2Vertex.clear();
  fVertex2ParentTrack.clear();
  if (! track ||  ! vertex) return;
  Int_t Nt = track->GetNRows();
  Int_t NV = vertex->GetNRows();
  g2t_track_st *t = track->GetTable();
  g2t_vertex_st *v = vertex->GetTable();
  for (Int_t i = 0; i < Nt; i++) {
    Int_t IdT = t[i].id - 1;                 assert(IdT >= 0 && IdT < Nt);
    Int_t IdV = t[i].start_vertex_p - 1;     assert(IdV >= 0 && IdV < NV);
    TVector3 Vx1(v[IdV].ge_x);
    Float_t tof1 = v[IdV].ge_tof;
    Int_t IdP = v[IdV].parent_p - 1;         
    if (IdP >= 0 && IdP < Nt) {
      Int_t IdVxP = t[IdP].start_vertex_p - 1; assert(IdVxP >= 0 && IdVxP < NV);
      TVector3 Vx2(v[IdVxP].ge_x);
      Vx2 -= Vx1;
      while ((Vx2.Mag() < 10e-4 && TMath::Abs(tof1-v[IdVxP].ge_tof) < 1e-9)) {
	IdV = IdVxP;
	IdP = v[IdV].parent_p - 1; if (IdP < 0) break;
	IdVxP = t[IdP].start_vertex_p - 1;  if (IdVxP < 0) break;
	Vx2 = TVector3(v[IdVxP].ge_x);
	Vx2 -= Vx1;
      }
    }
    fTrack2Vertex[IdT+1] = IdV+1;
  }
  for (Int_t i = 0; i < NV; i++) {
    fVertex2ParentTrack[v[i].id] = v[i].parent_p;
  }
}
