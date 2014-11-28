TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_MDFCorrection")) return 0;
  Int_t nrows = 6;
  MDFCorrection_st row;
  St_MDFCorrection *tableSet = new St_MDFCorrection("TpcLengthCorrectionMDF",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0 TPoints70BUGPRunXII14UU193; h2mdf("mu",5,1,20);  h2mdf("sigma",5,1,20) 
  //  row.nrows      = nrows;
  //  row.idx        =     1;// 
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60 
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 4 -> I 
  memset(&row,0,tableSet->GetRowSize()); // 0 TPointsBUGPRunXII14UU193; h2mdf("mu",5,1,20);  h2mdf("sigma",5,1,20) 
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
