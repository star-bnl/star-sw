TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 94081
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53138; // +/- 9.49485e-06 cm/us All: East = -1.80906 +/- 0.00169348
  row.laserDriftVelocityWest	 =   5.53138; // +/- 9.49485e-06 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53138 +/- 9.49485e-06
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.53138 +/- 9.49485e-06
};
