TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54745; // +/- 1.05454e-05 cm/us All: East = -0.279182 +/- 0.00533826
  row.laserDriftVelocityWest	 =   5.54745; // +/- 1.05454e-05 cm/us All: West = 0.260426 +/- 0.00202789
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54745 +/- 1.05454e-05
  return (TDataSet *)tableSet;// West = 5.54709 +/- 1.12859e-05 East = 5.54993 +/- 2.95988e-05
};
