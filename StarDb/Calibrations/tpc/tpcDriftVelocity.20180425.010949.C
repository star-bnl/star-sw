TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 114049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54181; // +/- 6.19597e-06 cm/us All: East = 1.16467 +/- 0.00507025
  row.laserDriftVelocityWest	 =   5.54181; // +/- 6.19597e-06 cm/us All: West = 1.97534 +/- 0.00108229
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54181 +/- 6.19597e-06
  return (TDataSet *)tableSet;// West = 5.54159 +/- 6.3494e-06 East = 5.5461 +/- 2.83562e-05
};
