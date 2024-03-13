#ifndef __CINT__
#include "tables/St_tpcCorrection_Table.h"
#endif

TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcCorrection")) return 0;
/* 
 Table: GatingGridB : parameters from Timothy Camarda, 08/09/21
 New Gating Grid for Run > 21: t0 = 240 ns, setting time = 2.0  us, tau = 2.0  / 4.6  = 435 ns.
 setting time = time of reaching transperency 99%
 tau = setting time/4.6 => exp(-4.6) = 1%
 description: Gating Grid transperancy = 0, for t < t0, and 1 - exp(-(t-t0)/tau), for t > t0
*/ 
  Int_t nrows = 1;
  St_tpcCorrection *tableSet = new St_tpcCorrection("GatingGridB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx           = 1;
  row.nrows         = nrows;
  row.npar          = 2;
  row.a[0]          = 0.24; // t0
  row.a[1]          = 2.00; // settingTime
  tableSet->AddAt(&row); // Outer
  return (TDataSet *)tableSet;
}
