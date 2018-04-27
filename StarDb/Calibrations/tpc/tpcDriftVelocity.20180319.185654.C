TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55321; // +/- 1.18781e-05 cm/us All: East = 0.149204 +/- 0.00398798
  row.laserDriftVelocityWest	 =   5.55321; // +/- 1.18781e-05 cm/us All: West = 0.220739 +/- 0.00255345
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55321 +/- 1.18781e-05
  return (TDataSet *)tableSet;// West = 5.55312 +/- 1.42164e-05 East = 5.55344 +/- 2.16181e-05
};
