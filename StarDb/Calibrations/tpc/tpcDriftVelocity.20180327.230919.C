TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86061
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5191; // +/- 6.82316e-06 cm/us All: East = -4.11002 +/- 8.7126
  row.laserDriftVelocityWest	 =   5.5191; // +/- 6.82316e-06 cm/us All: West = 0.197947 +/- 0.00121979
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5191 +/- 6.82316e-06
  return (TDataSet *)tableSet;// West = 5.5191 +/- 6.8241e-06 East = 5.52532 +/- 0.000411861
};
