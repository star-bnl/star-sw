TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55095; // +/- 1.6484e-05 cm/us All: East = -0.474007 +/- 0.00609973
  row.laserDriftVelocityWest	 =   5.55095; // +/- 1.6484e-05 cm/us All: West = 0.394864 +/- 0.00340598
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55095 +/- 1.6484e-05
  return (TDataSet *)tableSet;// West = 5.54983 +/- 1.88016e-05 East = 5.55466 +/- 3.42717e-05
};
