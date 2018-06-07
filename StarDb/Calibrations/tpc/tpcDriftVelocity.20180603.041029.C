TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55261; // +/- 5.74016e-06 cm/us All: East = 0.229603 +/- 0.00256056
  row.laserDriftVelocityWest	 =   5.55261; // +/- 5.74016e-06 cm/us All: West = 0.159286 +/- 0.0011098
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55261 +/- 5.74016e-06
  return (TDataSet *)tableSet;// West = 5.55267 +/- 6.26257e-06 East = 5.55229 +/- 1.43559e-05
};
