TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcGasTemperature",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;
  row.idx        =     1;
  row.npar       =            2;// outputGasTemperaturePGFHist004P05id
  row.a[0]	 = -2.64938e+00; 
  row.a[1]	 =  8.87511e-03;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            2;// outputGasTemperaturePGFHist004P05id
  row.a[0]	 = -2.64938e+00; 
  row.a[1]	 =  8.87511e-03;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}


