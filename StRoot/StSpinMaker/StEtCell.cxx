//////////////////////////////////////////////////////////////////////
//
// $Id: StEtCell.cxx,v 1.1 2002/02/11 20:30:47 akio Exp $
// $Log: StEtCell.cxx,v $
// Revision 1.1  2002/02/11 20:30:47  akio
// Many updates, including very first version of jet finder.
//
//
// Revision 1.0  2001/06/14 Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "StEventTypes.h"
#include "StEvent.h"
#include "StEtCell.h"

ClassImp(StEtCell);

void  StEtCell::print() const{
  printf("Key %6d    Eta %6.3f(%6.3f : %6.3f)    Phi %6.3f(%6.3f : %6.3f)  Entry %6d   Et %6.3f \n",
	 key,eta(),etaMin,etaMax,phi(),phiMin,phiMax,nEntry,et);
}

Int_t StEtCell::Compare(const TObject* obj) const{
  float et2 = ((StEtCell*)obj)->et;
  //  cout << "StEtCell::Compare" << et << " " << et2 << endl;
  if(et < et2) return -1;
  else if(et > et2) return 1;
  else return 0;
}

float  StEtCell::distanceEta(const StEtCell* othercell) const{
  return eta() - othercell->eta();
};

float  StEtCell::distancePhi(const StEtCell* othercell) const{
  float dphi = phi() - othercell->phi();
  while(dphi>M_PI) {dphi-=2*M_PI;}
  while(dphi<-M_PI) {dphi+=2*M_PI;}
  return dphi;
}

float  StEtCell::distance(const StEtCell* othercell) const{
  float deta = distanceEta(othercell);
  float dphi = distancePhi(othercell);
  return sqrt(deta*deta+dphi+dphi);
}

int StEtCell::check(int key,float e1,float e2,float p1,float p2) const{
  if(key!=key) return 2;
  if(e1!=etaMin) return 3;
  if(e2!=etaMax) return 4;
  if(p1!=phiMin) return 5;
  if(p2!=phiMax) return 6;
  return 0;
}


