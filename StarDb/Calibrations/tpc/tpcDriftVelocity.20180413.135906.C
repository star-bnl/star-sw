TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 103018
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54483; // +/- 1.73325e-05 cm/us All: East = -0.054324 +/- 0.0042852
  row.laserDriftVelocityWest	 =   5.54483; // +/- 1.73325e-05 cm/us All: West = 0.256843 +/- 0.00484279
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54483 +/- 1.73325e-05
  return (TDataSet *)tableSet;// West = 5.5439 +/- 2.65516e-05 East = 5.54552 +/- 2.288e-05
};
