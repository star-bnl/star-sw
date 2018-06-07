TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55085; // +/- 5.33741e-06 cm/us All: East = 0.0331078 +/- 0.00291686
  row.laserDriftVelocityWest	 =   5.55085; // +/- 5.33741e-06 cm/us All: West = 0.164578 +/- 0.000992518
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55085 +/- 5.33741e-06
  return (TDataSet *)tableSet;// West = 5.55078 +/- 5.64277e-06 East = 5.55149 +/- 1.64479e-05
};
