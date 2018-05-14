TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 127026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.57078; // +/- 8.38003e-06 cm/us All: East = -0.188869 +/- 0.00207584
  row.laserDriftVelocityWest	 =   5.57078; // +/- 8.38003e-06 cm/us All: West = 0.246293 +/- 0.00217983
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.57078 +/- 8.38003e-06
  return (TDataSet *)tableSet;// West = 5.56958 +/- 1.20997e-05 East = 5.57189 +/- 1.16174e-05
};
