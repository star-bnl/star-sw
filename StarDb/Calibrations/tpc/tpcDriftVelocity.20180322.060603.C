TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55007; // +/- 9.03659e-06 cm/us All: East = -0.0703536 +/- 28.1754
  row.laserDriftVelocityWest	 =   5.55007; // +/- 9.03659e-06 cm/us All: West = 0.173513 +/- 0.00161885
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55007 +/- 9.03659e-06
  return (TDataSet *)tableSet;// West = 5.55007 +/- 9.03659e-06 East = 5.27931 +/- 1.80806
};
