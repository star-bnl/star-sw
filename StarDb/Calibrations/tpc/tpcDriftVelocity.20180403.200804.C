TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 93042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52058; // +/- 9.00568e-06 cm/us All: East = -0.734857 +/- 0.00632296
  row.laserDriftVelocityWest	 =   5.52058; // +/- 9.00568e-06 cm/us All: West = 0.236217 +/- 0.00167269
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52058 +/- 9.00568e-06
  return (TDataSet *)tableSet;// West = 5.52024 +/- 9.30823e-06 East = 5.52558 +/- 3.5612e-05
};
