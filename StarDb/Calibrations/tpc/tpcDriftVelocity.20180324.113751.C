TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 83018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.524; // +/- 9.69839e-06 cm/us All: East = 1.54469 +/- 0.014574
  row.laserDriftVelocityWest	 =   5.524; // +/- 9.69839e-06 cm/us All: West = 2.02819 +/- 0.0017649
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.524 +/- 9.69839e-06
  return (TDataSet *)tableSet;// West = 5.52396 +/- 9.77244e-06 East = 5.52652 +/- 7.89284e-05
};
