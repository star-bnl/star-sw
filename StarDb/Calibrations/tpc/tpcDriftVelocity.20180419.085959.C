TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 109013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55363; // +/- 0.000184286 cm/us All: East = -0.373658 +/- 0.681416
  row.laserDriftVelocityWest	 =   5.55363; // +/- 0.000184286 cm/us All: West = 0.527705 +/- 0.143937
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55363 +/- 0.000184286
  return (TDataSet *)tableSet;// West = 5.55363 +/- 0.000184305 East = 5.56307 +/- 0.0130017
};
