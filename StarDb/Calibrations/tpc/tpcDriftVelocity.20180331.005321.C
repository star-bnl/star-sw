TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89060
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52621; // +/- 9.96911e-06 cm/us All: East = -0.511602 +/- 0.00808185
  row.laserDriftVelocityWest	 =   5.52621; // +/- 9.96911e-06 cm/us All: West = 0.23696 +/- 0.00183206
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52621 +/- 9.96911e-06
  return (TDataSet *)tableSet;// West = 5.52602 +/- 1.02186e-05 East = 5.53006 +/- 4.53912e-05
};
