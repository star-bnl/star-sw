TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55713; // +/- 3.08922e-05 cm/us All: East = 0.000873125 +/- 0.00537218
  row.laserDriftVelocityWest	 =   5.55713; // +/- 3.08922e-05 cm/us All: West = 0.546048 +/- 0.0118268
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55713 +/- 3.08922e-05
  return (TDataSet *)tableSet;// West = 5.55572 +/- 0.00016825 East = 5.55718 +/- 3.14265e-05
};
