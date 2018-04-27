TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5367; // +/- 1.2143e-05 cm/us All: East = 2.15331 +/- 0.0243295
  row.laserDriftVelocityWest	 =   5.5367; // +/- 1.2143e-05 cm/us All: West = 2.01165 +/- 0.00217111
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5367 +/- 1.2143e-05
  return (TDataSet *)tableSet;// West = 5.53671 +/- 1.22604e-05 East = 5.53611 +/- 8.79571e-05
};
