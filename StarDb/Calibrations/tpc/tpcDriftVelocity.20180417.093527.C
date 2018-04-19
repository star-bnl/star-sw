TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55558; // +/- 1.32064e-05 cm/us All: East = -0.177526 +/- 0.0035711
  row.laserDriftVelocityWest	 =   5.55558; // +/- 1.32064e-05 cm/us All: West = 0.216016 +/- 0.00327887
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55558 +/- 1.32064e-05
  return (TDataSet *)tableSet;// West = 5.55459 +/- 1.81008e-05 East = 5.55672 +/- 1.93112e-05
};
