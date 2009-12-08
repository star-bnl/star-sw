TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcAcCharge")) return 0;
  tpcAcCharge_st row =  {4.2015,1.87026};
  St_tpcAcCharge *tableSet = new St_tpcAcCharge("tpcAcCharge",1);
  tableSet->AddAt(&row.chargeI, 0);
  return (TDataSet *)tableSet;
}
