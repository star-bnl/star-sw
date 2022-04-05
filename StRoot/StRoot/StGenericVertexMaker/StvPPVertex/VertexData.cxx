#include <stdio.h>
#include <cmath>
#include "math_constants.h"

#include <StMessMgr.h>

#include "VertexData.h"
namespace StEvPPV {
//==========================================================
//==========================================================
VertexData::VertexData() {
  id=0;
  r=TVector3(999,999,999);
  gPtSum=0;
  nUsedTrack=Lmax=nBtof=nBtofV=0;
  nAnyMatch=nCtb=nBemc=nEemc=nTpc=0;
  nAnyVeto=nCtbV=nBemcV=nEemcV=nTpcV=0;
}


//==========================================================
//==========================================================
void VertexData::print(ostream& os) const { // does not work ??
  os <<"  Vertex ID="<<id<<" nUsedTrack="<<nUsedTrack<<" gPtSum="<< gPtSum<<" Lmax="<< Lmax;
  os <<" match: any="<<nAnyMatch<<"-"<<nAnyVeto<<" CTB="<<nCtb<<"-"<<nCtbV<<" BEMC="<<nBemc<<"-"<<nBemcV<<" EEMC="<<nEemc<<"-"<<nEemcV<<" TPC="<<nTpc<<"-"<<nTpcV;
 os <<"  Vz="<<r.z()<<" +/-"<<er.z()<<endl;
}
}// end namespace StEvPPV
