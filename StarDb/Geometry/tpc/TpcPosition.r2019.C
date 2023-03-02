TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_Survey")) return 0;
  Survey_st row = {
    //           -gamma     beta    gamma            -alpha    -beta    alpha                 x0        y0        z0
    0,       1,-0.00036,-0.00048, 0.00036,       1, -0.0001, 0.00048,  0.0001,       1,  -0.2383,  -0.1732,  -0.1957,0,0,0,0,0,0,"2013 Tpc"
  };
  St_Survey *tableSet = new St_Survey("TpcPosition",1);
  tableSet->AddAt(&row.Id);
  return (TDataSet *) tableSet;
}
