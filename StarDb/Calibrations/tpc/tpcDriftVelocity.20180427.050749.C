TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 117003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54331; // +/- 0.000200785 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.54331; // +/- 0.000200785 cm/us All: West = 0.0904821 +/- 0.0506271
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54331 +/- 0.000200785
  return (TDataSet *)tableSet;// West = 5.54331 +/- 0.000200785 East = -999 +/- 999
};
