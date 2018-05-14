TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55038; // +/- 5.50245e-06 cm/us All: East = -0.0488177 +/- 0.00333915
  row.laserDriftVelocityWest	 =   5.55038; // +/- 5.50245e-06 cm/us All: West = 0.207494 +/- 0.00102308
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55038 +/- 5.50245e-06
  return (TDataSet *)tableSet;// West = 5.55025 +/- 5.77317e-06 East = 5.55163 +/- 1.81819e-05
};
