TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 54027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5094; // +/- 3.65701e-05 cm/us All: East = 0.0682367 +/- 0.0121861
  row.laserDriftVelocityWest	 =   5.5094; // +/- 3.65701e-05 cm/us All: West = 0.224797 +/- 0.00921926
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5094 +/- 3.65701e-05
  return (TDataSet *)tableSet;// West = 5.50907 +/- 4.47608e-05 East = 5.51006 +/- 6.34212e-05
};
