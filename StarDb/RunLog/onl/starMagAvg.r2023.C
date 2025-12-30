TDataSet *CreateTable() {
  if (!TClass::GetClass("St_starMagAvg")) return 0;
  starMagAvg_st row = {22145028,1621962080,1621964191,0, 4500,0};
  St_starMagAvg *tableSet = new St_starMagAvg("starMagAvg",1);
  tableSet->AddAt(&row.runNumber);
  return (TDataSet *)tableSet;
}
