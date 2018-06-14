TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163040
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54878; // +/- 6.82947e-06 cm/us All: East = -0.479162 +/- 0.00470061
  row.laserDriftVelocityWest	 =   5.54878; // +/- 6.82947e-06 cm/us All: West = 0.191403 +/- 0.00124942
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54878 +/- 6.82947e-06
  return (TDataSet *)tableSet;// West = 5.54851 +/- 7.08461e-06 East = 5.55225 +/- 2.56795e-05
};
