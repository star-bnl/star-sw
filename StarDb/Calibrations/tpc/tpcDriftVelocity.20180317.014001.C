TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53539; // +/- 1.08706e-05 cm/us All: East = 0.82734 +/- 3.48799
  row.laserDriftVelocityWest	 =   5.53539; // +/- 1.08706e-05 cm/us All: West = 2.04767 +/- 0.00194623
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53539 +/- 1.08706e-05
  return (TDataSet *)tableSet;// West = 5.53539 +/- 1.08709e-05 East = 5.53562 +/- 0.00163108
};
