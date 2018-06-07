TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5507; // +/- 0.000144564 cm/us All: East = -0.199414 +/- 0.321191
  row.laserDriftVelocityWest	 =   5.5507; // +/- 0.000144564 cm/us All: West = 0.177852 +/- 0.127815
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5507 +/- 0.000144564
  return (TDataSet *)tableSet;// West = 5.55055 +/- 0.000148356 East = 5.55359 +/- 0.000643586
};
