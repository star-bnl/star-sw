TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5487; // +/- 5.22217e-06 cm/us All: East = 0.108798 +/- 0.00156432
  row.laserDriftVelocityWest	 =   5.5487; // +/- 5.22217e-06 cm/us All: West = 0.192867 +/- 0.00115651
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5487 +/- 5.22217e-06
  return (TDataSet *)tableSet;// West = 5.54853 +/- 6.50888e-06 East = 5.54901 +/- 8.7488e-06
};
