TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 75055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54335; // +/- 1.02074e-05 cm/us All: East = 0.88688 +/- 1.27662
  row.laserDriftVelocityWest	 =   5.54335; // +/- 1.02074e-05 cm/us All: West = 0.187227 +/- 0.00182591
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54335 +/- 1.02074e-05
  return (TDataSet *)tableSet;// West = 5.54335 +/- 1.02149e-05 East = 5.54233 +/- 0.000266608
};
