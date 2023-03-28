TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_FilePath")) return 0;
  St_FilePath *tableSet = new St_FilePath("Path2tpxGain",1);
  FilePath_st row = {0};
  strncpy(row.file,"tpx/tpx_gains.txt.from21",24);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
