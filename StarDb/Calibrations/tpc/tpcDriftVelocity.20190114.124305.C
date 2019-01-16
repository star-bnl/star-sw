TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 14022
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54946; // +/- 2.39997e-05 cm/us All: East = -0.749988 +/- 0.0595197
  row.laserDriftVelocityWest	 =   5.54946; // +/- 2.39997e-05 cm/us All: West = 0.00455481 +/- 0.00431229
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54946 +/- 2.39997e-05
  return (TDataSet *)tableSet;// West = 5.54933 +/- 2.44066e-05 East = 5.55344 +/- 0.000131984
};
