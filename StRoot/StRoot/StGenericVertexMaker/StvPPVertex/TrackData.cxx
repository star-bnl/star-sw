#include <stdio.h>
#include <cmath>
#include <StMessMgr.h>

#include "TrackData.h"
#include "VertexData.h"

namespace StEvPPV {
//==========================================================
//==========================================================
TrackData::TrackData() {
  ctbBin=-1; 
  bemcBin=-1; 
  eemcBin=-1; 
  vertexID=0;
  gPt=0;
  anyMatch=anyVeto=false;
  mBtof=mCtb=mBemc=mEemc=mTpc=0;
  weight=1;
  zDca=ezDca=rxyDca=0;
  mother=0;
}


//==========================================================
//==========================================================
bool 
TrackData::matchVertex(VertexData &V, float dzMax) {

  float dz=zDca-V.r.z();
  bool ret= fabs(dz) < dzMax+ezDca;
  if(ret)   LOG_DEBUG<< Form("PPV::matchTr2Ver VerID=%d  weight=%.2f anyM=%d anyV=%d  m: ctb=%d  bemc=%d eemc=%d tpc=%d dz=%.2f +/- %.2f\n",V.id,weight,anyMatch,anyVeto,mCtb,mBemc,mEemc,mTpc,dz,ezDca)<<endm;
  return ret;
}


//==========================================================
//==========================================================
void 
TrackData::scanNodes(vector<int> &hit, int jz0){
  /* INPUT: vector of hits for active nodes
     i=[0,jz0-1] is on one side of z-Axis
     i=[jz0,mx-1] on another side
  */

  // params
  const int minCenter=4 , minMiss=6; // criteria for Match & Veto
  const int mxDev=2; // max # of deviations from expected pattern

  // printf("patt size=%d, jz0=%d\n",hit.size(),jz0);

  int nPatt[2];
  int i;
  bool vetoL=false, vetoR=false, matchL=false, matchR=false;

  // Scan Left end for missing hits
  memset(nPatt,0,sizeof(nPatt));
  for(i=0;i<(int)hit.size();i++) {
    // printf("i=%d hit=%d\n",i,hit[i]);
    nPatt[hit[i]]++;
    if(nPatt[1]>mxDev) break;
    if(nPatt[0]<minMiss) continue;
    vetoL=true;
    break;
  }

  // printf("vetoL=%d   nUp=%d nDwn=%d\n\n",vetoL, nPatt[1],nPatt[0]);

  // Scan Right end for missing hits
  memset(nPatt,0,sizeof(nPatt));
  for(i=hit.size()-1; i>=0;i--) {
    // printf("i=%d hit=%d\n",i,hit[i]);
    nPatt[hit[i]]++;
    if(nPatt[1]>mxDev) break;
    if(nPatt[0]<minMiss) continue;
    vetoR=true;
    break;
  }

  // printf("vetoR=%d nUp=%d nDwn=%d\n\n",vetoR, nPatt[1],nPatt[0]);

  if(jz0>minCenter && jz0<(int)hit.size()-minCenter) { // examin membrane
    // Scan Left half at membrane
    memset(nPatt,0,sizeof(nPatt));
    for(i=jz0-1; i>=0;i--) {
      //  printf("i=%d hit=%d\n",i,hit[i]);
      nPatt[hit[i]]++;
      if(nPatt[0]>mxDev) break;
      if(nPatt[1]<minCenter) continue;
      matchL=true;
      break;
    }
    
    // printf("matchL=%d   nUp=%d nDwn=%d\n\n",matchL, nPatt[1],nPatt[0]);
    
    
    // Scan Right half at membrane
    memset(nPatt,0,sizeof(nPatt));
    for(i=jz0;i<(int)hit.size();i++) {
      // printf("i=%d hit=%d\n",i,hit[i]);
      nPatt[hit[i]]++;
      if(nPatt[0]>mxDev) break;
      if(nPatt[1]<minCenter) continue;
      matchR=true;
      break;
    }
    
    // printf("matchR=%d   nUp=%d nDwn=%d\n\n",matchR, nPatt[1],nPatt[0]);
    
  } // end of membrane test

  bool match=  matchL && matchR;
  bool veto=(vetoL || vetoR) && !match;

  //  printf(" TPC Conclusion: tpcMatch=%d  tpcVeto=%d \n\n",match,veto);
  updateAnyMatch(match,veto,mTpc);
  weight*=getTpcWeight();
}

//==========================================================
//==========================================================
void
TrackData::updateAnyMatch(bool match, bool veto, int & mXXX){
  if(match) {
    anyMatch=true;
    anyVeto=false;
    mXXX=1;
    //  } else if(veto && (!anyMatch) ) {
  } else if(veto && (!match) ) {
    anyVeto=true;
    mXXX=-1;
  } else {
    mXXX=0;
  }
}

//==========================================================
//==========================================================
float
TrackData:: getTpcWeight(){
  const float Wdunno=1, Wmatch=5, Wveto=0.2;
  if(mTpc>0) return Wmatch;  
  if(mTpc<0) return Wveto;
  return Wdunno;
}
}// end namespace StEvPPV
