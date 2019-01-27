TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 26049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53811; // +/- 1.47112e-05 cm/us All: East = 3.93569 +/- 0.00903414
  row.laserDriftVelocityWest	 =   5.53811; // +/- 1.47112e-05 cm/us All: West = 3.86611 +/- 0.00274722
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53811 +/- 1.47112e-05
  return (TDataSet *)tableSet;// West = 5.53816 +/- 1.53007e-05 East = 5.53742 +/- 5.35152e-05
};
