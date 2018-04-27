TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 77009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55508; // +/- 0.000298673 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.55508; // +/- 0.000298673 cm/us All: West = -2.54605 +/- 2.22235
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55508 +/- 0.000298673
  return (TDataSet *)tableSet;// West = 5.55508 +/- 0.000298673 East = -999 +/- 999
};
