TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 6;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMDF",nrows);
  memset(&row,0,tableSet->GetRowSize()); //  TPoints70UGPRunXVII14; h2mdf("mu",5,1,20);        
  for (Int_t i = 0; i < nrows; i++) {
    tableSet->AddAt(&row);// 0 -> I70                                                            
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
