TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 16039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55748; // +/- 1.74364e-05 cm/us All: East = -0.090282 +/- 0.0105384
  row.laserDriftVelocityWest	 =   5.55748; // +/- 1.74364e-05 cm/us All: West = 0.360856 +/- 0.00337214
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55748 +/- 1.74364e-05
  return (TDataSet *)tableSet;// West = 5.55721 +/- 1.84758e-05 East = 5.55969 +/- 5.2727e-05
};
