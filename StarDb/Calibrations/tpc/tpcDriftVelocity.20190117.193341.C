TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 17050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55865; // +/- 9.30581e-06 cm/us All: East = -0.481304 +/- 0.00274949
  row.laserDriftVelocityWest	 =   5.55865; // +/- 9.30581e-06 cm/us All: West = 0.0876829 +/- 0.00215857
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55865 +/- 9.30581e-06
  return (TDataSet *)tableSet;// West = 5.55745 +/- 1.1709e-05 East = 5.56072 +/- 1.53326e-05
};
