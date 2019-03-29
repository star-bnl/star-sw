TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row = {
    //             -gamma      beta     gamma              -alpha     -beta     alpha                 x0       y0       z0
    1, 1.000000, 0.000000, 0.000000, 0.000000, 1.000000, 0.000000, 0.000000, 0.000000, 1.000000,  0.0000,  0.0000,  0.0000,0,0,0,0,0,0,"Ideal"
  };
  St_Survey *tableSet = new St_Survey("TpcSuperSectorPositionB",24);
  for (Int_t i = 0; i < 24; i++) { row.Id = i+1; tableSet->AddAt(&row.Id, i);}
  return (TDataSet *)tableSet;
}
