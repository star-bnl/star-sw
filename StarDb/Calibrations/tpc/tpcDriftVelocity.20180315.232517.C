TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74101
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53197; // +/- 9.61653e-06 cm/us All: East = -0.176551 +/- 0.00874155
  row.laserDriftVelocityWest	 =   5.53197; // +/- 9.61653e-06 cm/us All: West = 0.184791 +/- 0.0017662
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53197 +/- 9.61653e-06
  return (TDataSet *)tableSet;// West = 5.53189 +/- 9.80754e-06 East = 5.53399 +/- 4.89633e-05
};
