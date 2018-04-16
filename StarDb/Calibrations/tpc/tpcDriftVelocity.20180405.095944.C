TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52187; // +/- 2.46513e-05 cm/us All: East = -0.267478 +/- 0.0187433
  row.laserDriftVelocityWest	 =   5.52187; // +/- 2.46513e-05 cm/us All: West = -0.0663553 +/- 0.00553889
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52187 +/- 2.46513e-05
  return (TDataSet *)tableSet;// West = 5.52152 +/- 2.76888e-05 East = 5.5232 +/- 5.41342e-05
};
