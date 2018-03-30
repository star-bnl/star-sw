TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52693; // +/- 1.40183e-05 cm/us All: East = 1.85435 +/- 0.00763219
  row.laserDriftVelocityWest	 =   5.52693; // +/- 1.40183e-05 cm/us All: West = 2.82992 +/- 0.00270257
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52693 +/- 1.40183e-05
  return (TDataSet *)tableSet;// West = 5.52636 +/- 1.48327e-05 East = 5.53169 +/- 4.28954e-05
};
