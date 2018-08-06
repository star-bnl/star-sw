TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_TpcAvgCurrent")) return 0;
  TpcAvgCurrent_st row;
  memset(row, 0, sizeof(TpcAvgCurrent_st));
  St_TpcAvgCurrent *tableSet = new St_TpcAvgCurrent("TpcAvgCurrent",1);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
