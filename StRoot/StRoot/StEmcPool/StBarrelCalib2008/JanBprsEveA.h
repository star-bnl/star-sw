#ifndef STAR_JanBprsEveA_H
#define STAR_JanBprsEveA_H

#include <TGraph.h>

#include "JanBarrelConst.h"
class  BprsCapPolygraph;
class JanBprsEveA {
 friend  class BprsCapPolygraph;
 public:
 JanBprsEveA(){ } //printf("cstr:JanBprsEve\n"); }
  void clear() { chi2=chi2dof=eveID=capID=crateID=-999; raw.Set(0); good.Set(0);
  bestChi2dof=bestCapID=-888; useFix=false;
  }
  int getBestCapID() { if(useFix) return bestCapID; else  return capID; }
  void print() {
    printf("BPRS: eveID=%d capID=%d carteID=%d nRaw=%d  nGood=%d chi2=%f chi2dof=%f\n",eveID,capID,crateID,raw.GetN(),good.GetN(),chi2,chi2dof);
    if(bestCapID>=0) printf("       better capID=%d chi2dof=%f useFix=%d\n", bestCapID,bestChi2dof, useFix);
    // raw.Print();
  }
  void set(int cap, int crate, int e) { capID=cap; crateID=crate; eveID=e;}
  void addRawValue(double id, float rawAdc) { raw.SetPoint(raw.GetN(),rawAdc,id);}
  void addGoodValue(double id, float rawAdc) { good.SetPoint(good.GetN(),rawAdc,id);}

 private:
  int capID, crateID,eveID,bestCapID, useFix;
  float chi2,chi2dof ;// base values for nominal capID
  float bestChi2dof;
  TGraph raw; // X=rawADC, y=id; all 1200 ADC
  TGraph good; // X=rawADC, y=id; only good channels

};

#endif
