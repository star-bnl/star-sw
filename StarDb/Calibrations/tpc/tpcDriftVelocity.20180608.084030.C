TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55024; // +/- 4.90133e-06 cm/us All: East = 0.281321 +/- 0.00185208
  row.laserDriftVelocityWest	 =   5.55024; // +/- 4.90133e-06 cm/us All: West = 0.188138 +/- 0.000985733
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55024 +/- 4.90133e-06
  return (TDataSet *)tableSet;// West = 5.55035 +/- 5.60439e-06 East = 5.54988 +/- 1.01073e-05
};
