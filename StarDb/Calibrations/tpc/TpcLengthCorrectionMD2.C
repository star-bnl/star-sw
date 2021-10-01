TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 1;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMD2",nrows);
  memset(&row,0,tableSet->GetRowSize()); //  TPoints70UGPRunXVII14; h2mdf("mu",5,1,20);        
  tableSet->AddAt(&row);// 5 -> sigma.Ifit	     
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
