TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 152009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55057; // +/- 6.92741e-05 cm/us All: East = -0.0898942 +/- 0.413132
  row.laserDriftVelocityWest	 =   5.55057; // +/- 6.92741e-05 cm/us All: West = 0.120117 +/- 0.0596226
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55057 +/- 6.92741e-05
  return (TDataSet *)tableSet;// West = 5.54946 +/- 0.000101868 East = 5.55153 +/- 9.44851e-05
};
