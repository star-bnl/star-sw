//*-- Author : Jan Balewski
// $Id: StppDst.h,v 1.1 2001/04/19 21:40:09 balewski Exp $
// $Log: StppDst.h,v $
// Revision 1.1  2001/04/19 21:40:09  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:19:09  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//
//  This class 
//  is used to store the DST info relevant for the ppSpin analysis 
//                                                                    
//////////////////////////////////////////////////////////////////////////

#include "TTable.h"

typedef struct {
  float pt; // (GeV/c) pT of recon LP
  float eta; // (1) pseudorapidity of  recon LP
  float psi; // (deg) azimuth recon LP
  int nTclHit; // number of TPC hits for LP
  float Dz; // (cm)  recVert - recLP
  float DRxy; // (cm) R(dx,dy) recVert - recLP
  float Rxy;  // (cm) R(x,y) recLP at DCA to recVer
  float chi2f; // chi2/DOF for rec LP
  int PrimId;  // of rec LP for evaluation only
  int nPrim;  // total number of prim tracks found bt ppLMV
  float vertX; // (cm) vertex found by ppLMV
  float vertY,vertZ;
} ppDst_t;

class ppDst: public TTable {
 public:
  ClassDefTable(ppDst,ppDst_t)
  ClassDef(ppDst,1)
};


