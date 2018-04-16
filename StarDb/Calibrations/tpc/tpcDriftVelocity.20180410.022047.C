TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55258; // +/- 2.16276e-05 cm/us All: East = -5.62422 +/- 0.00489088
  row.laserDriftVelocityWest	 =   5.55258; // +/- 2.16276e-05 cm/us All: West = -4.95077 +/- 0.00758414
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55258 +/- 2.16276e-05
  return (TDataSet *)tableSet;// West = 5.54992 +/- 3.94558e-05 East = 5.55372 +/- 2.58585e-05
};
