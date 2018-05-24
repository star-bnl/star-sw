TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53822; // +/- 2.0933e-05 cm/us All: East = 0.053613 +/- 0.105633
  row.laserDriftVelocityWest	 =   5.53822; // +/- 2.0933e-05 cm/us All: West = 0.139552 +/- 0.0040315
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53822 +/- 2.0933e-05
  return (TDataSet *)tableSet;// West = 5.53824 +/- 2.11052e-05 East = 5.53685 +/- 0.000164215
};
