#include "TRandom.h"
 
#include "StVertex.h"
 
 
ClassImp(StVertex)
 
 
//______________________________________________________________________________
StVertex::StVertex(const Char_t  *name) : TNamed()
{
  // Create an StVertex object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  SetName(name);
  mNtrack = 0;
  mType   = 0;          // Vertex type
  mQualityBitmask = 0;  // bitmask of e.g. quality information
  xPosition = 0;     
  yPosition = 0;
  zPosition = 0;

  xPositionError = 0;
  yPositionError = 0;
  zPositionError = 0;
  mChiSquared    = 0;
}
 
//______________________________________________________________________________
StVertex::~StVertex()
{
   Clear();
}

//______________________________________________________________________________
StVertex &StVertex::Assign(dst_vertex_st & vertex)
{
  // Create an StVertex object.
  // When the constructor is invoked for the first time, the class static
  // variable fgTracks is 0 and the TClonesArray fgTracks is created.
  //  if (name && strlen(name)) SetName(name);
  mNtrack         = 0;
  mType           = vertex.vtx_id; // Vertex type
  mQualityBitmask = vertex.iflag;  // bitmask of e.g. quality information
  xPosition       = vertex.x;     
  yPosition       = vertex.y;     
  zPosition       = vertex.z;     

  xPositionError  = vertex.sigma[0];
  yPositionError  = vertex.sigma[1];
  zPositionError  = vertex.sigma[2];
  mChiSquared     = vertex.pchi2;
  return *this;
}
//______________________________________________________________________________
StVertex &StVertex::operator=(dst_vertex_st & vertex)
{
  //   Assign(vertex,this->GetName());
   Assign(vertex);
   return *this;
}
//______________________________________________________________________________
void StVertex::Clear(Option_t *option)
{

}
 

