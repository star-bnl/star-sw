TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98058
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55106; // +/- 6.26357e-05 cm/us All: East = -5.46129 +/- 0.0387616
  row.laserDriftVelocityWest	 =   5.55106; // +/- 6.26357e-05 cm/us All: West = -4.98204 +/- 0.0299806
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55106 +/- 6.26357e-05
  return (TDataSet *)tableSet;// West = 5.55019 +/- 7.73189e-05 East = 5.55272 +/- 0.000106833
};
