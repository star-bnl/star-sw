TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54361; // +/- 5.88989e-06 cm/us All: East = -0.0118167 +/- 0.00199984
  row.laserDriftVelocityWest	 =   5.54361; // +/- 5.88989e-06 cm/us All: West = 0.188 +/- 0.00122504
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54361 +/- 5.88989e-06
  return (TDataSet *)tableSet;// West = 5.5433 +/- 6.89871e-06 East = 5.54444 +/- 1.13125e-05
};
