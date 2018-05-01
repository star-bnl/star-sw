TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53512; // +/- 0.000136828 cm/us All: East = -0.0203718 +/- 0.239724
  row.laserDriftVelocityWest	 =   5.53512; // +/- 0.000136828 cm/us All: West = 0.246034 +/- 0.215923
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53512 +/- 0.000136828
  return (TDataSet *)tableSet;// West = 5.53498 +/- 0.000154643 East = 5.53565 +/- 0.000293643
};
