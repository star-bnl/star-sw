TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 122019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53189; // +/- 8.20573e-06 cm/us All: East = -0.344288 +/- 0.00273603
  row.laserDriftVelocityWest	 =   5.53189; // +/- 8.20573e-06 cm/us All: West = 0.292319 +/- 0.00173558
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53189 +/- 8.20573e-06
  return (TDataSet *)tableSet;// West = 5.53093 +/- 9.68027e-06 East = 5.53433 +/- 1.54675e-05
};
