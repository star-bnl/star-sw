TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92084
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52957; // +/- 2.3323e-05 cm/us All: East = -1.13692 +/- 0.0127099
  row.laserDriftVelocityWest	 =   5.52957; // +/- 2.3323e-05 cm/us All: West = 0.0750056 +/- 0.00476903
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52957 +/- 2.3323e-05
  return (TDataSet *)tableSet;// West = 5.52867 +/- 2.51183e-05 East = 5.53522 +/- 6.28203e-05
};
