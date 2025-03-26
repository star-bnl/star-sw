TDataSet *CreateTable() {
  if (!TClass::GetClass("St_starMagAvg")) return 0;
  starMagAvg_st row = { 0, 0, 01, 0, 0, 0};
  St_starMagAvg *tableSet = new St_starMagAvg("starMagAvg",1);
  tableSet->AddAt(&row.runNumber);
  return (TDataSet *)tableSet;
}
