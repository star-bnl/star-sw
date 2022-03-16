#include <stdio.h>
#include <cmath>
#include "math_constants.h"

#include <St_base/StMessMgr.h>

#include "StGenericVertexMaker/StiPPVertex/VertexData.h"
//==========================================================
//==========================================================
VertexData::VertexData(int vertexId) {
  id=vertexId;
  isTriggered = false;
  mIdTruth=0;
  r=TVector3(999,999,999);
  gPtSum=0;
  nUsedTrack=Lmax=nBtof=nBtofV=0;
  nAnyMatch=nCtb=nBemc=nEemc=nTpc=0;
  nAnyVeto=nCtbV=nBemcV=nEemcV=nTpcV=0;
}


VertexData::VertexData(const TVector3& position) :
  id(0),
  isTriggered(false),
  mIdTruth(0),
  r(position), er(),
  nUsedTrack(0), Lmax(0), gPtSum(0),
  nBtof(0),  nCtb(0),  nBemc(0),  nEemc(0),  nTpc(0),  nAnyMatch(0),
  nBtofV(0), nCtbV(0), nBemcV(0), nEemcV(0), nTpcV(0), nAnyVeto(0)
{
}

//==========================================================
//==========================================================
void VertexData::print(ostream& os) const { // does not work ??
  os << " Vertex ID="<<id<< " isTriggered: " << isTriggered << " nUsedTrack="<<nUsedTrack<<" gPtSum="<< gPtSum<<" Lmax="<< Lmax << " idTruth: " << mIdTruth
     << " match: any="<<nAnyMatch<<"-"<<nAnyVeto<<" CTB="<<nCtb<<"-"<<nCtbV<<" BEMC="<<nBemc<<"-"<<nBemcV<<" EEMC="<<nEemc<<"-"<<nEemcV<<" TPC="<<nTpc<<"-"<<nTpcV << "\n"
     << Form(" xyz: (%5.3f, %5.3f, %5.3f) +/- (%5.3f, %5.3f, %5.3f)\n", r.x(), r.y(), r.z(), er.x(), er.y(), er.z() );
}
