TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcGasTemperature",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;
  row.idx        =     1;
  row.npar       =            0;// outputGasTemperaturePGFHist004P05id
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            0;// outputGasTemperaturePGFHist004P05id
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}


