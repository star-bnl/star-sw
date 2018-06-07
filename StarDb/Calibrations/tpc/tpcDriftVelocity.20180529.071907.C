TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55101; // +/- 4.86744e-06 cm/us All: East = 0.208695 +/- 0.00173915
  row.laserDriftVelocityWest	 =   5.55101; // +/- 4.86744e-06 cm/us All: West = 0.151258 +/- 0.00098722
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55101 +/- 4.86744e-06
  return (TDataSet *)tableSet;// West = 5.55109 +/- 5.63323e-06 East = 5.5508 +/- 9.66926e-06
};
