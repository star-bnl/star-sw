TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53699; // +/- 1.0228e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.53699; // +/- 1.0228e-05 cm/us All: West = 0.381498 +/- 0.00180211
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53699 +/- 1.0228e-05
  return (TDataSet *)tableSet;// West = 5.53699 +/- 1.0228e-05 East = -999 +/- 999
};
