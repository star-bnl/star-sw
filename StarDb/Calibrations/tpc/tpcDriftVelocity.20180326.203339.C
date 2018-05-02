TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 85043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52934; // +/- 7.48828e-05 cm/us All: East = 0.0254445 +/- 1.10096
  row.laserDriftVelocityWest	 =   5.52934; // +/- 7.48828e-05 cm/us All: West = 0.0996047 +/- 0.064085
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52934 +/- 7.48828e-05
  return (TDataSet *)tableSet;// West = 5.52934 +/- 7.49124e-05 East = 5.53172 +/- 0.00266422
};
