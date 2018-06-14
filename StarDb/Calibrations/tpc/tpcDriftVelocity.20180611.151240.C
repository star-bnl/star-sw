TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54755; // +/- 9.86523e-06 cm/us All: East = 0.2215 +/- 0.00496662
  row.laserDriftVelocityWest	 =   5.54755; // +/- 9.86523e-06 cm/us All: West = 0.680542 +/- 0.00210313
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54755 +/- 9.86523e-06
  return (TDataSet *)tableSet;// West = 5.54718 +/- 1.07249e-05 East = 5.54958 +/- 2.51479e-05
};
