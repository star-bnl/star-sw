TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 68054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53186; // +/- 4.45943e-05 cm/us All: East = 5.76805 +/- 0.042382
  row.laserDriftVelocityWest	 =   5.53186; // +/- 4.45943e-05 cm/us All: West = 6.56369 +/- 0.0142194
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53186 +/- 4.45943e-05
  return (TDataSet *)tableSet;// West = 5.53074 +/- 5.16176e-05 East = 5.53517 +/- 8.85507e-05
};
