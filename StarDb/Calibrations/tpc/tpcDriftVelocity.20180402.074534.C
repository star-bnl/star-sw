TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51823; // +/- 1.56849e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.51823; // +/- 1.56849e-05 cm/us All: West = 0.222922 +/- 0.00284507
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51823 +/- 1.56849e-05
  return (TDataSet *)tableSet;// West = 5.51823 +/- 1.56849e-05 East = -999 +/- 999
};
