#ifndef VertexData_h
#define VertexData_h
/**
 * full description of found vertex
 */

#include <TVector3.h>

class VertexData {
 public:
  int id; // vertex ID assigned by PPV
  bool isTriggered; ///< Indicates whether the vertex potentially belongs to triggered event
  short mIdTruth;
  TVector3 r,er; // vertex position and its error
  int nUsedTrack; // # of tracks used to identify the vertex
  float Lmax; // maximum of the likelhood function.
  float gPtSum; // total tranverse momentum of used tracks.
  int nBtof,nCtb,nBemc,nEemc,nTpc,nAnyMatch; // number of matched tracks
  int nBtofV,nCtbV,nBemcV,nEemcV,nTpcV,nAnyVeto; // number of vetoed tracks
  
  // methods
  VertexData(int vertexId=0);
  VertexData(const TVector3& position);
  void print(ostream& os) const;
};
#endif
