//*-- Author : Jan Balewski
// 3/30/01 - make it self inserting 
// 
// $Id: StppMiniDst.h,v 1.1.1.2 2001/04/21 00:43:14 fisyak Exp $
// $Log: StppMiniDst.h,v $
// Revision 1.1.1.2  2001/04/21 00:43:14  fisyak
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:13  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//
//  This class is self inserting to the StEvent
//  It is used to store the DST info relevant for the ppSpin analysis 
//                                                                    
//////////////////////////////////////////////////////////////////////////
/* contents:

   ppMDST.rLP.pt,eta,psi  (pt<0 flags invalid 'rLP' record)
	     .nTclHit
	     .Dz,DRxy,Rxy,chi2f
	     .nPrim
	     .TrId 
	     .vert.x,y,z
	 .gLP.pt,eta,psi   (pt<0 flags invalid 'gLP' record)
	     .good,match,ng2tHit
	     .Dpsi
	     .vert.x,y,z
*/

#include "StObject.h"

class StMaker;

struct StppMiniDst_Vert {
  float x,y,z; //(cm)
};

struct StppMiniDst_GLP {
  float pt,eta,psi; //units: GeV/c, 1, deg
  int ng2tHit;
  float Dpsi;
  int good,match;
  struct StppMiniDst_Vert vert;
};

struct StppMiniDst_RLP {
  float pt,eta,psi; //units: GeV/c, 1, deg
  int nTclHit;
  float Dz,DRxy,Rxy;
  float chi2f;
  int PrimId;  // for evaluation only
  int nPrim;
  struct StppMiniDst_Vert vert;
};

class StppMiniDst : public StObject {
 public:
  StppMiniDst();
  static StppMiniDst * GetppMiniDst(StMaker *);
  struct StppMiniDst_GLP gLP;
  struct StppMiniDst_RLP rLP;
  ClassDef(StppMiniDst,1)
};
