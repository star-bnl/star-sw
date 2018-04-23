TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52922; // +/- 7.87313e-06 cm/us All: East = -0.56241 +/- 0.00617812
  row.laserDriftVelocityWest	 =   5.52922; // +/- 7.87313e-06 cm/us All: West = 0.187306 +/- 0.00144386
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52922 +/- 7.87313e-06
  return (TDataSet *)tableSet;// West = 5.52902 +/- 8.08169e-06 East = 5.53292 +/- 3.48808e-05
};
