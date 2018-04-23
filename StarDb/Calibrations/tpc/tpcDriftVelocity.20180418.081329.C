TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 108009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5554; // +/- 1.8035e-05 cm/us All: East = 0.0728301 +/- 0.00393405
  row.laserDriftVelocityWest	 =   5.5554; // +/- 1.8035e-05 cm/us All: West = 0.52333 +/- 0.00690073
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5554 +/- 1.8035e-05
  return (TDataSet *)tableSet;// West = 5.55357 +/- 3.71495e-05 East = 5.55596 +/- 2.06291e-05
};
