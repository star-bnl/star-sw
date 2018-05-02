TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 56035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51245; // +/- 9.35601e-06 cm/us All: East = -0.0153337 +/- 0.00269235
  row.laserDriftVelocityWest	 =   5.51245; // +/- 9.35601e-06 cm/us All: West = 0.365142 +/- 0.00222163
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51245 +/- 9.35601e-06
  return (TDataSet *)tableSet;// West = 5.51162 +/- 1.2049e-05 East = 5.5137 +/- 1.48479e-05
};
