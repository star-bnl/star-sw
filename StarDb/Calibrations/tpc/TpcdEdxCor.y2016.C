TDataSet *CreateTable() { // Ad hoc accont of saturation in dE/dx
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdEdxCor",1);
  memset(&row,0,tableSet->GetRowSize()); 
   tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
