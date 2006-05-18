#include "Hybrids.h"
TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/svt/svtDriftCorrection
//  Table: svtCorrection_st[0]--> svtCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_svtCorrection")) return 0;
  Int_t NT = 432;
  St_svtCorrection *tableSet = new St_svtCorrection("svtDriftCorrection",NT);
  svtCorrection_st row;
  for (Int_t i = 0; i < NT; i++) {
    memset (&row,0,tableSet->GetRowSize());
    row.Npar = 0;
    row.barrel = hybrids[i].Barrel;
    row.ladder = hybrids[i].Ladder;
    row.layer  = 2*row.barrel - 1 + row.ladder%2;
    row.wafer  = hybrids[i].Wafer;
    row.hybrid = hybrids[i].Hybrid;
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
