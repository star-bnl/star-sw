TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 125009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54481; // +/- 6.23786e-06 cm/us All: East = 0.210627 +/- 0.0019438
  row.laserDriftVelocityWest	 =   5.54481; // +/- 6.23786e-06 cm/us All: West = 0.177155 +/- 0.00134757
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54481 +/- 6.23786e-06
  return (TDataSet *)tableSet;// West = 5.54487 +/- 7.61055e-06 East = 5.54469 +/- 1.08884e-05
};
