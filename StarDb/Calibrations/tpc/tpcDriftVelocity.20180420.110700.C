TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54355; // +/- 7.38e-06 cm/us All: East = -0.0519722 +/- 0.00215891
  row.laserDriftVelocityWest	 =   5.54355; // +/- 7.38e-06 cm/us All: West = 0.210835 +/- 0.00106453
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54355 +/- 7.38e-06
  return (TDataSet *)tableSet;// West = 5.54283 +/- 9.32972e-06 East = 5.54476 +/- 1.2063e-05
};
