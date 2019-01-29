TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 28053
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52743; // +/- 9.10479e-06 cm/us All: East = 0.264906 +/- 0.00371163
  row.laserDriftVelocityWest	 =   5.52743; // +/- 9.10479e-06 cm/us All: West = 0.190592 +/- 0.00179646
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52743 +/- 9.10479e-06
  return (TDataSet *)tableSet;// West = 5.52756 +/- 1.00944e-05 East = 5.52686 +/- 2.10856e-05
};
