//*-- Author : Jan Balewski
//  
// $Id: StppMiniDst.h,v 1.2 2001/02/28 19:06:13 balewski Exp $
// $Log: StppMiniDst.h,v $
// Revision 1.2  2001/02/28 19:06:13  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//  Store the DST info relevant for the ppSpin analysis                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
/* contents:

   ppMDST.Tslat
         .CtbAdcSumChan
         .data1,data2
	 .polDir={+,-,0,x}
	 .rLP.pt,eta,psi
	     .nTclHit
	     .Dz,DRxy,Rxy,chi2f
	 .gLP.pt,eta,psi
	     .good,match,ng2tHit
	     .Dpsi
	 .rV.x,y,z
	    .nPrim
	 .gV.x,y,z
*/

#include "StObject.h"

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

class StppMiniDst : public StObject {
 public:
  StppMiniDst();
  static StppMiniDst * GetppMiniDst(StMaker *);
  int Tslat;// accounts for changed RHIC rings optics
  int  CtbAdcSumChan;
  struct StppMiniDst_GVert gvert;
  struct StppMiniDst_RVert rvert;
  struct StppMiniDst_GLP gLP;
  struct StppMiniDst_RLP rLP;
  PolDir polDir;
  int data1;
  int data2;
  ClassDef(StppMiniDst,1)
};
