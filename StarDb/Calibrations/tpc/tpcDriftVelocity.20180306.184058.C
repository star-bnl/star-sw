TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 65033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5313; // +/- 8.62226e-06 cm/us All: East = -0.341075 +/- 0.00249781
  row.laserDriftVelocityWest	 =   5.5313; // +/- 8.62226e-06 cm/us All: West = 0.50826 +/- 0.00198703
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5313 +/- 8.62226e-06
  return (TDataSet *)tableSet;// West = 5.52948 +/- 1.10262e-05 East = 5.53417 +/- 1.38331e-05
};
