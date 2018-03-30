TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52641; // +/- 5.00059e-05 cm/us All: East = 2.96587 +/- 30.5283
  row.laserDriftVelocityWest	 =   5.52641; // +/- 5.00059e-05 cm/us All: West = 2.79207 +/- 0.0140624
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52641 +/- 5.00059e-05
  return (TDataSet *)tableSet;// West = 5.52641 +/- 5.00059e-05 East = 5.54139 +/- 0.171791
};
