TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcAcCharge")) return 0;
  tpcAcCharge_st row =  {2.50093,1.0551};
  St_tpcAcCharge *tableSet = new St_tpcAcCharge("tpcAcCharge",1);
  tableSet->AddAt(&row.chargeI, 0);
  return (TDataSet *)tableSet;
}
