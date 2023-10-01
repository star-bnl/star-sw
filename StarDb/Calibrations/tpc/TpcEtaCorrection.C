#include "tables/St_tpcCorrection_Table.h"

TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrection",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize()); // /hlt/cephfs/fisyak/TpcRS_2021.COL.NoPad/Fit2 $ root.exe Eta3G4EYpion.root
  tableSet->AddAt(&row);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
