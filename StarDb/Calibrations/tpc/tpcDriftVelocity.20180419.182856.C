TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5571; // +/- 6.33103e-06 cm/us All: East = -0.623055 +/- 0.00221544
  row.laserDriftVelocityWest	 =   5.5571; // +/- 6.33103e-06 cm/us All: West = -0.377026 +/- 0.00129997
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5571 +/- 6.33103e-06
  return (TDataSet *)tableSet;// West = 5.55675 +/- 7.35661e-06 East = 5.5581 +/- 1.24309e-05
};
