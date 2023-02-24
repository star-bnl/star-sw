TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_FilePath")) return 0;
  St_FilePath *tableSet = new St_FilePath("Path2itpcGain",1);
  FilePath_st row = {0};
  strncpy(row.file,"itpc/itpc_gains.txt.11Sep19.1",30);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
