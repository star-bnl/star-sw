TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52812; // +/- 5.60038e-05 cm/us All: East = -2.9612 +/- 0.263187
  row.laserDriftVelocityWest	 =   5.52812; // +/- 5.60038e-05 cm/us All: West = -2.0604 +/- 0.0196867
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52812 +/- 5.60038e-05
  return (TDataSet *)tableSet;// West = 5.52776 +/- 5.84321e-05 East = 5.53227 +/- 0.000196305
};
