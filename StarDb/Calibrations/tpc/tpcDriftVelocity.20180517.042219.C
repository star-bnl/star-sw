TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53845; // +/- 1.46935e-05 cm/us All: East = 0.227657 +/- 0.00625262
  row.laserDriftVelocityWest	 =   5.53845; // +/- 1.46935e-05 cm/us All: West = 0.127062 +/- 0.00303492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53845 +/- 1.46935e-05
  return (TDataSet *)tableSet;// West = 5.5385 +/- 1.65382e-05 East = 5.53824 +/- 3.2015e-05
};
