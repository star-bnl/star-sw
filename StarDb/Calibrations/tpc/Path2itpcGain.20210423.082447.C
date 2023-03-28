TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_FilePath")) return 0;
  St_FilePath *tableSet = new St_FilePath("Path2itpcGain",1);
  FilePath_st row = {0};
  strncpy(row.file,"itpc/itpc_gains.txt.May14_21.bak",32);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
