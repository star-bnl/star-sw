TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 94057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52044; // +/- 1.02578e-05 cm/us All: East = -0.16593 +/- 0.00369469
  row.laserDriftVelocityWest	 =   5.52044; // +/- 1.02578e-05 cm/us All: West = 0.270312 +/- 0.00217057
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52044 +/- 1.02578e-05
  return (TDataSet *)tableSet;// West = 5.51985 +/- 1.18832e-05 East = 5.52217 +/- 2.03192e-05
};
