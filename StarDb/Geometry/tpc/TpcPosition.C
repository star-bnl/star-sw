TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {
    //           -gamma     beta    gamma            -alpha    -beta    alpha                 x0        y0        z0
    0, 1.00000, 0.00000, 0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.00000, 1.00000,   0.0000,   0.0000,   0.0000,0,0,0,0,0,0,"Ideal Tpc"
  };
  Int_t n = 1;// east = 0, west = 1
  St_Survey *tableSet = new St_Survey("TpcPosition",n);
  tableSet->AddAt(&row.Id);
  return (TDataSet *)tableSet;
}
