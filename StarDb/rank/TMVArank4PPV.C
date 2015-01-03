TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_TMVArank")) return 0;
  St_TMVArank *tableSet = new St_TMVArank("TMVArank4PPV",1);
  TMVArank_st row = {"","",""};
  tableSet->AddAt(&row);
 return (TDataSet *)tableSet;
}
