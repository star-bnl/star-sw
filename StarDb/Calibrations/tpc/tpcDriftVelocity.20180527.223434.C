TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55023; // +/- 4.64784e-06 cm/us All: East = 0.0352232 +/- 0.00326922
  row.laserDriftVelocityWest	 =   5.55023; // +/- 4.64784e-06 cm/us All: West = 0.183832 +/- 0.000842413
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55023 +/- 4.64784e-06
  return (TDataSet *)tableSet;// West = 5.55018 +/- 4.80613e-06 East = 5.55088 +/- 1.82601e-05
};
