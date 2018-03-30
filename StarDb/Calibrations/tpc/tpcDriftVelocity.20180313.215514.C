TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 72030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53814; // +/- 2.43878e-05 cm/us All: East = 0.0721757 +/- 0.0178373
  row.laserDriftVelocityWest	 =   5.53814; // +/- 2.43878e-05 cm/us All: West = 0.759342 +/- 0.00461569
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53814 +/- 2.43878e-05
  return (TDataSet *)tableSet;// West = 5.53779 +/- 2.5531e-05 East = 5.5417 +/- 8.24243e-05
};
