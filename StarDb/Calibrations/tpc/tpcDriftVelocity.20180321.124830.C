TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54942; // +/- 1.67951e-05 cm/us All: East = -2.44285 +/- 0.0173618
  row.laserDriftVelocityWest	 =   5.54942; // +/- 1.67951e-05 cm/us All: West = -1.2771 +/- 0.00318938
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54942 +/- 1.67951e-05
  return (TDataSet *)tableSet;// West = 5.54909 +/- 1.72437e-05 East = 5.55553 +/- 7.41136e-05
};
