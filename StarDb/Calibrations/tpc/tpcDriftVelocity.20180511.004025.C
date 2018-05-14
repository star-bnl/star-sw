TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130076
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.551; // +/- 4.75267e-05 cm/us All: East = 0.202787 +/- 0.037592
  row.laserDriftVelocityWest	 =   5.551; // +/- 4.75267e-05 cm/us All: West = 0.122889 +/- 0.0126457
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.551 +/- 4.75267e-05
  return (TDataSet *)tableSet;// West = 5.55088 +/- 5.59386e-05 East = 5.55131 +/- 9.01164e-05
};
