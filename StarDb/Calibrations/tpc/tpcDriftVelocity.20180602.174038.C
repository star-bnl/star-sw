TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 153036
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5525; // +/- 5.77394e-06 cm/us All: East = 0.0569023 +/- 0.00310073
  row.laserDriftVelocityWest	 =   5.5525; // +/- 5.77394e-06 cm/us All: West = 0.0720049 +/- 0.00108563
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5525 +/- 5.77394e-06
  return (TDataSet *)tableSet;// West = 5.55249 +/- 6.12516e-06 East = 5.55256 +/- 1.73e-05
};
