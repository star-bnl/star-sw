TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_FilePath")) return 0;
  St_FilePath *tableSet = new St_FilePath("Path2tpxGain",1);
  FilePath_st row = {0};
  strncpy(row.file,"tpx/tpc_gains.txt.20091022.061431",33);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
