TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 149055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54991; // +/- 6.50694e-06 cm/us All: East = -0.31083 +/- 0.00526544
  row.laserDriftVelocityWest	 =   5.54991; // +/- 6.50694e-06 cm/us All: West = 0.214115 +/- 0.00117432
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54991 +/- 6.50694e-06
  return (TDataSet *)tableSet;// West = 5.54977 +/- 6.67869e-06 East = 5.55268 +/- 2.88787e-05
};
