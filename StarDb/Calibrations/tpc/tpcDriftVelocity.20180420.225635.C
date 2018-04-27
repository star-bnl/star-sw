TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 110041
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54401; // +/- 5.22692e-06 cm/us All: East = -0.0423311 +/- 0.00283614
  row.laserDriftVelocityWest	 =   5.54401; // +/- 5.22692e-06 cm/us All: West = 0.21577 +/- 0.000970561
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54401 +/- 5.22692e-06
  return (TDataSet *)tableSet;// West = 5.54386 +/- 5.53694e-06 East = 5.54522 +/- 1.58428e-05
};
