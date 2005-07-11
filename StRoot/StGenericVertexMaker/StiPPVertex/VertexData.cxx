#include <stdio.h>
#include <cmath>
#include "math_constants.h"

#include <StMessMgr.h>

#include "VertexData.h"
//==========================================================
//==========================================================
VertexData::VertexData() {
  id=0;
  r=TVector3(999,999,999);
  gPtSum=0;
  nAnyMatch=nCtb=nBemc=nEemc=nTpc=0;
  nAnyVeto=nCtbV=nBemcV=nEemcV=nTpcV=0;
}


//==========================================================
//==========================================================
void VertexData::print(ostream& os) { // does not work ??
  os <<"  Vertex ID="<<id<<" nUsedTrack="<<nUsedTrack<<" gPtSum="<< gPtSum<<" Lmax="<< Lmax;
  os <<" match: any="<<nAnyMatch<<"-"<<nAnyVeto<<" CTB="<<nCtb<<"-"<<nCtbV<<" BEMC="<<nBemc<<"-"<<nBemcV<<" EEMC="<<nEemc<<"-"<<nEemcV<<" TPC="<<nTpc<<"-"<<nTpcV;

  // os <<"    Vx=%.2f"<<v.x()<<" +/-"<<ev.x() <<"  Vy=%.2f"<<v.y()<<" +/-"<<ev.y() <<"  Vz=%.2f"<<v.z()<<" +/-"<<ev.z()<<endl;
 os <<"  Vz="<<r.z()<<" +/-"<<er.z()<<endl;
}
