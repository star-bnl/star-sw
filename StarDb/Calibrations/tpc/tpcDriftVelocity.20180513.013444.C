TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132079
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54745; // +/- 0.000148793 cm/us All: East = -0.0743577 +/- 0.637593
  row.laserDriftVelocityWest	 =   5.54745; // +/- 0.000148793 cm/us All: West = 0.123325 +/- 0.0840872
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54745 +/- 0.000148793
  return (TDataSet *)tableSet;// West = 5.54705 +/- 0.000181205 East = 5.5483 +/- 0.000260701
};
