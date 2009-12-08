TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcAcCharge")) return 0;
  tpcAcCharge_st row =  {28.3497,9.54113};
  St_tpcAcCharge *tableSet = new St_tpcAcCharge("tpcAcCharge",1);
  tableSet->AddAt(&row.chargeI, 0);
  return (TDataSet *)tableSet;
}
