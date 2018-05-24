TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139058
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55876; // +/- 4.74727e-06 cm/us All: East = -0.0867304 +/- 0.00224337
  row.laserDriftVelocityWest	 =   5.55876; // +/- 4.74727e-06 cm/us All: West = 0.164937 +/- 0.000901327
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55876 +/- 4.74727e-06
  return (TDataSet *)tableSet;// West = 5.55855 +/- 5.12227e-06 East = 5.55999 +/- 1.26399e-05
};
