TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81058
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5423; // +/- 1.51126e-05 cm/us All: East = 0.262841 +/- 0.0123053
  row.laserDriftVelocityWest	 =   5.5423; // +/- 1.51126e-05 cm/us All: West = 0.2044 +/- 0.00276636
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5423 +/- 1.51126e-05
  return (TDataSet *)tableSet;// West = 5.54232 +/- 1.56043e-05 East = 5.54211 +/- 6.06841e-05
};
