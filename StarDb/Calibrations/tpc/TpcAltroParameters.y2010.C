TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_TpcAltroParameters")) return 0;
  St_TpcAltroParameters *tableSet = new St_TpcAltroParameters("TpcAltroParameters",24);
  TpcAltroParameters_st row;
  row.N            =       6;
  row.altro_reg[0] =   57158; //K1 coefficient of the TCF
  row.altro_reg[1] =   21787; //K2 coefficient of the TCF
  row.altro_reg[2] =   26699; //K3 coefficient of the TCF
  row.altro_reg[3] =    7449; //L1 coefficient of the TCF
  row.altro_reg[4] =   37911; //L2 coefficient of the TCF
  row.altro_reg[5] =   58775; //L3 coefficient of the TCF
  for (Int_t i = 0; i < 24; i++) {
    tableSet->AddAt(&row);
  }
  return (TDataSet *)tableSet;
}
