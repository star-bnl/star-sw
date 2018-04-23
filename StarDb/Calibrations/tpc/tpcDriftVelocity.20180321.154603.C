TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80065
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54812; // +/- 1.1268e-05 cm/us All: East = -0.483803 +/- 0.00696174
  row.laserDriftVelocityWest	 =   5.54812; // +/- 1.1268e-05 cm/us All: West = 0.258706 +/- 0.00213063
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54812 +/- 1.1268e-05
  return (TDataSet *)tableSet;// West = 5.54775 +/- 1.18385e-05 East = 5.55169 +/- 3.674e-05
};
