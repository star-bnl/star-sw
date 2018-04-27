TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 107014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54546; // +/- 1.27661e-05 cm/us All: East = -0.225576 +/- 0.00334008
  row.laserDriftVelocityWest	 =   5.54546; // +/- 1.27661e-05 cm/us All: West = 0.133592 +/- 0.00323319
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54546 +/- 1.27661e-05
  return (TDataSet *)tableSet;// West = 5.54454 +/- 1.774e-05 East = 5.54645 +/- 1.83854e-05
};
