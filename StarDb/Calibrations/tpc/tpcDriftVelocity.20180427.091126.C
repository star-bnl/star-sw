TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 117011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54268; // +/- 0.00020021 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54268; // +/- 0.00020021 cm/us All: West = 0.304458 +/- 0.0843758
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54268 +/- 0.00020021
  return (TDataSet *)tableSet;// West = 5.54268 +/- 0.00020021 East = -999 +/- 999
};
