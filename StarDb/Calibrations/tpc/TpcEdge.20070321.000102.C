TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEdge",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows       =       nrows;//
  row.idx         =           1;//
  row.type        =         200;//  
  row.npar        =           0;// cut 
  row.min         =           5;// distance to edge > 5
  row.max         =         500;
  tableSet->AddAt(&row);
  row.idx         =           2;//
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
