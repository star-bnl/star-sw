TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcAvCurrent")) return 0;
  tpcAvCurrent_st row;
  St_tpcAvCurrent *tableSet = new St_tpcAvCurrent("tpcAvCurrent",1);
  memset (&row, 0, sizeof(tpcAvCurrent_st));
  tableSet->AddAt(&row.run, 0);
  return (TDataSet *)tableSet;
}
