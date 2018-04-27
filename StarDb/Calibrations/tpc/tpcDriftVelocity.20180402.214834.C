TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92084
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51948; // +/- 2.3095e-05 cm/us All: East = -0.79701 +/- 0.0140354
  row.laserDriftVelocityWest	 =   5.51948; // +/- 2.3095e-05 cm/us All: West = 0.368011 +/- 0.00459992
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51948 +/- 2.3095e-05
  return (TDataSet *)tableSet;// West = 5.51874 +/- 2.45785e-05 East = 5.52504 +/- 6.7498e-05
};
