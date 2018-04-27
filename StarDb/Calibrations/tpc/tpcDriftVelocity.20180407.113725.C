TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97024
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5354; // +/- 7.61704e-06 cm/us All: East = 0.0918005 +/- 0.00203792
  row.laserDriftVelocityWest	 =   5.5354; // +/- 7.61704e-06 cm/us All: West = 0.325748 +/- 0.00183658
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5354 +/- 7.61704e-06
  return (TDataSet *)tableSet;// West = 5.53483 +/- 1.02675e-05 East = 5.5361 +/- 1.13593e-05
};
