//*-- Author : Mirko Planinic
// $Id: StppEmcDst.h,v 1.1 2001/05/03 23:38:10 balewski Exp $
// $Log: StppEmcDst.h,v $
// Revision 1.1  2001/05/03 23:38:10  balewski
// *** empty log message ***
//
//
//
//////////////////////////////////////////////////////////////////////////
//
//  This class 
//  is used to store the DST info relevant for the ppEmcSpin analysis 
//                                                                    
//////////////////////////////////////////////////////////////////////////

#include "TTable.h"

typedef struct {
  float Eta; // Eta position of the cluster
  float Phi; // Phi position of the cluster
  float SigmaR; // R over which the cluster is spread
  float Energy; // energy of the EMC cluster
  float TrackMom;  // momentum of the closest charged particle
  float DeltaR;  // distance of the closest charged particle to the cluster
  int trackFlag; // =1 if char.part.within (R) distance of the clust. position
  int modFlag; // =1 if cluster is spread over more than one module
} ppEmcDst_t;

class ppEmcDst: public TTable {
 public:
  ClassDefTable(ppEmcDst,ppEmcDst_t)
  ClassDef(ppEmcDst,1)
};
