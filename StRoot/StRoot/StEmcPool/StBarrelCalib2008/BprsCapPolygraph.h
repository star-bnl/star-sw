#ifndef STAR_BprsCapPolygraph_h
#define STAR_BprsCapPolygraph_h

#include "JanBprsEveA.h"
class TH2F;
class TObjArray ;
class StJanBarrelDbMaker;
class JanBarrelEvent;

class BprsCapPolygraph {
 public:
  BprsCapPolygraph( TObjArray *HList,  StJanBarrelDbMaker* ,int pedFlag=3);
  void doBaseline(JanBprsEveA & bprsEve, JanBarrelEvent &fullEve);
  void findBestCap(JanBprsEveA & bprsEve, JanBarrelEvent &fullEve);
  void doPedResidua(JanBprsEveA & bprsEve);

  //----------------------
  void  setCut(float a, float b, int c,float d){ cut_adcMax=a; cut_fracSkip=b; par_mxDelCap=c; cut_stepChi2dof=d;}

  //----------------------
  void print(){
    printf("BprsPoly PAR:  mxDelCap=%d, CUT: maxAdc=%.1f fracSkip=%.3f stepChi2dof=%.1f\n", par_mxDelCap, cut_adcMax,cut_fracSkip,cut_stepChi2dof);
  }

 private:
    float cut_adcMax, cut_fracSkip, cut_stepChi2dof;
    int  par_mxDelCap; 
    int par_pedFlag;
    StJanBarrelDbMaker *mJanDbMaker;

 public:
    TH2F *hCh2D, *hCapGood, *hCapCorr;
    TH1F  *hAdcGood, *hAdcCorr, *hChiB, *hChiGap, *hChiGap2;

};
#endif
