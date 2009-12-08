TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcAcCharge")) return 0;
  tpcAcCharge_st row =  {2.00072,0.868206};
  St_tpcAcCharge *tableSet = new St_tpcAcCharge("tpcAcCharge",1);
  tableSet->AddAt(&row.chargeI, 0);
  return (TDataSet *)tableSet;
}
