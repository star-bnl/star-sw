TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54122; // +/- 1.39486e-05 cm/us All: East = 0.245765 +/- 0.00309353
  row.laserDriftVelocityWest	 =   5.54122; // +/- 1.39486e-05 cm/us All: West = 0.582783 +/- 0.00432739
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54122 +/- 1.39486e-05
  return (TDataSet *)tableSet;// West = 5.54001 +/- 2.37558e-05 East = 5.54186 +/- 1.72318e-05
};
