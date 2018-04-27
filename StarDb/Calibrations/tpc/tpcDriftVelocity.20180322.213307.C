TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53417; // +/- 1.30802e-05 cm/us All: East = 1.7523 +/- 0.011287
  row.laserDriftVelocityWest	 =   5.53417; // +/- 1.30802e-05 cm/us All: West = 1.8946 +/- 0.00238828
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53417 +/- 1.30802e-05
  return (TDataSet *)tableSet;// West = 5.53413 +/- 1.33889e-05 East = 5.53495 +/- 6.12689e-05
};
