TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 123033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53108; // +/- 8.4687e-06 cm/us All: East = -0.043364 +/- 0.00248139
  row.laserDriftVelocityWest	 =   5.53108; // +/- 8.4687e-06 cm/us All: West = 0.376778 +/- 0.00194383
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53108 +/- 8.4687e-06
  return (TDataSet *)tableSet;// West = 5.53023 +/- 1.07114e-05 East = 5.53249 +/- 1.38309e-05
};
