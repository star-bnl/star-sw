TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51981; // +/- 9.43961e-06 cm/us All: East = -0.44426 +/- 0.00598265
  row.laserDriftVelocityWest	 =   5.51981; // +/- 9.43961e-06 cm/us All: West = 0.291571 +/- 0.00176037
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51981 +/- 9.43961e-06
  return (TDataSet *)tableSet;// West = 5.51949 +/- 9.82698e-06 East = 5.52353 +/- 3.39553e-05
};
