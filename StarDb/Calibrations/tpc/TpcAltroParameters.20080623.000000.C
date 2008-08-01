TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_TpcAltroParameters")) return 0;
  St_TpcAltroParameters *tableSet = new St_TpcAltroParameters("TpcAltroParameters",24);
  TpcAltroParameters_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.N = 6;
  row.altro_reg[0]  = 60882 ;//   RC.altro_reg[ALTRO_K1] = 60882 ;
  row.altro_reg[1]  = 36241 ;//   RC.altro_reg[ALTRO_K2] = 36241 ;
  row.altro_reg[2]  = 16777 ;//   RC.altro_reg[ALTRO_K3] = 16777 ;
  
  row.altro_reg[3]  = 61931 ;//   RC.altro_reg[ALTRO_L1] = 61931 ;
  row.altro_reg[4]  = 44236 ;//   RC.altro_reg[ALTRO_L2] = 44236 ;
  row.altro_reg[5]  =  3644 ;//   RC.altro_reg[ALTRO_L3] =  3644 ;
  for (Int_t i = 0; i < 24; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
