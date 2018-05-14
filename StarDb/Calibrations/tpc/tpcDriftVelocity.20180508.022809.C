TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 127046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56523; // +/- 1.06444e-05 cm/us All: East = -0.192003 +/- 0.00337772
  row.laserDriftVelocityWest	 =   5.56523; // +/- 1.06444e-05 cm/us All: West = 0.334097 +/- 0.00227901
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56523 +/- 1.06444e-05
  return (TDataSet *)tableSet;// West = 5.5643 +/- 1.29534e-05 East = 5.56715 +/- 1.86792e-05
};
