TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 84063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5312; // +/- 1.11356e-05 cm/us All: East = 1.50041 +/- 0.0109791
  row.laserDriftVelocityWest	 =   5.5312; // +/- 1.11356e-05 cm/us All: West = 1.96829 +/- 0.00205578
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5312 +/- 1.11356e-05
  return (TDataSet *)tableSet;// West = 5.5311 +/- 1.13626e-05 East = 5.53366 +/- 5.59834e-05
};
