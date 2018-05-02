TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 120014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53255; // +/- 0.000159602 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.53255; // +/- 0.000159602 cm/us All: West = 0.454129 +/- 0.137782
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53255 +/- 0.000159602
  return (TDataSet *)tableSet;// West = 5.53255 +/- 0.000159602 East = -999 +/- 999
};
