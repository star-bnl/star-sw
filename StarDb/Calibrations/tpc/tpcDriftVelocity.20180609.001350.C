TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55039; // +/- 5.36435e-06 cm/us All: East = -0.651616 +/- 0.00414883
  row.laserDriftVelocityWest	 =   5.55039; // +/- 5.36435e-06 cm/us All: West = 0.111957 +/- 0.000976763
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55039 +/- 5.36435e-06
  return (TDataSet *)tableSet;// West = 5.55016 +/- 5.51684e-06 East = 5.55442 +/- 2.29741e-05
};
