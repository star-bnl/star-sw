TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 140016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55855; // +/- 5.67326e-06 cm/us All: East = 0.0853642 +/- 0.00320792
  row.laserDriftVelocityWest	 =   5.55855; // +/- 5.67326e-06 cm/us All: West = 0.324212 +/- 0.00106093
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55855 +/- 5.67326e-06
  return (TDataSet *)tableSet;// West = 5.55844 +/- 5.97975e-06 East = 5.55962 +/- 1.79509e-05
};
