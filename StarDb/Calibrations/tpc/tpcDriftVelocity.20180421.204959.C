TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55416; // +/- 1.88881e-05 cm/us All: East = -0.661137 +/- 0.0113031
  row.laserDriftVelocityWest	 =   5.55416; // +/- 1.88881e-05 cm/us All: West = 0.218957 +/- 0.00366208
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55416 +/- 1.88881e-05
  return (TDataSet *)tableSet;// West = 5.55387 +/- 1.95204e-05 East = 5.55853 +/- 7.48196e-05
};
