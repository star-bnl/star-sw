TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52336; // +/- 1.08552e-05 cm/us All: East = 0.00601977 +/- 0.00267451
  row.laserDriftVelocityWest	 =   5.52336; // +/- 1.08552e-05 cm/us All: West = 0.385348 +/- 0.00279598
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52336 +/- 1.08552e-05
  return (TDataSet *)tableSet;// West = 5.52226 +/- 1.56902e-05 East = 5.52437 +/- 1.50339e-05
};
