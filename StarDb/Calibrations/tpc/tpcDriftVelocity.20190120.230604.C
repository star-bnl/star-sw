TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 20049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55766; // +/- 9.11002e-06 cm/us All: East = -0.705285 +/- 0.00364712
  row.laserDriftVelocityWest	 =   5.55766; // +/- 9.11002e-06 cm/us All: West = -0.133948 +/- 0.00179952
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55766 +/- 9.11002e-06
  return (TDataSet *)tableSet;// West = 5.55706 +/- 1.01472e-05 East = 5.56016 +/- 2.06845e-05
};
