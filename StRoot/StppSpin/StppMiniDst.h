//JB

enum PolDir{voidPol=0, upPol, downPol, noPol};
class StMaker;

struct StppMiniDst_GVert {
  float x,y,z; //(cm)
};

struct StppMiniDst_RVert {
  float x,y,z; //(cm)
  int nPrim;
};

struct StppMiniDst_GLP {
  float pt,eta,psi; //units: GeV/c, 1, deg
  int ng2tHit;
  float Dpsi;
  int good,match;
};

struct StppMiniDst_RLP {
  float pt,eta,psi; //units: GeV/c, 1, deg
  int nTclHit;
  float Dz,DRxy,Rxy;
  float chi2f;
};

#include "StObject.h"
class StppMiniDst : public StObject {
 public:
  StppMiniDst();
  static StppMiniDst * GetppMiniDst(StMaker *);
  int  CtbAdcSumChan;
  struct StppMiniDst_GVert gvert;
  struct StppMiniDst_RVert rvert;
  struct StppMiniDst_GLP gLP;
  struct StppMiniDst_RLP rLP;
  enum PolDir polDir;
  int data1;
  int data2;
  ClassDef(StppMiniDst,1)
};
