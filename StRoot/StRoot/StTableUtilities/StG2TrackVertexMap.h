#ifndef __StG2TrackVertexMap_h__
#define __StG2TrackVertexMap_h__
#include <map>
#include "Rtypes.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
class StG2TrackVertexMap {
 public:
  static  StG2TrackVertexMap* instance(St_g2t_track *track=0, St_g2t_vertex *vertex = 0);
  void    Reset(St_g2t_track *track, St_g2t_vertex *vertex);
  virtual ~StG2TrackVertexMap() {fgInstance = 0;}
  Int_t   IdVertex(Int_t IdTrack) {return fTrack2Vertex.empty() ? -1 : fTrack2Vertex.find(IdTrack)->second;}
  Int_t   IdParentTrack(Int_t IdVertex) {return fVertex2ParentTrack.empty() ? -1 : fVertex2ParentTrack.find(IdVertex)->second;}
 private:
  StG2TrackVertexMap(St_g2t_track *track=0, St_g2t_vertex *vertex = 0) : fTrack(track), fVertex(vertex) {Reset(track,vertex);}
  St_g2t_track *fTrack;
  St_g2t_vertex *fVertex;
  std::map<Int_t,Int_t> fTrack2Vertex;
  std::map<Int_t,Int_t> fVertex2ParentTrack;
  static StG2TrackVertexMap* fgInstance;
};
#endif
