TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 106016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55473; // +/- 1.56673e-05 cm/us All: East = -0.187501 +/- 1.58649
  row.laserDriftVelocityWest	 =   5.55473; // +/- 1.56673e-05 cm/us All: West = 0.0581432 +/- 0.00282255
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55473 +/- 1.56673e-05
  return (TDataSet *)tableSet;// West = 5.55473 +/- 1.56675e-05 East = 5.55569 +/- 0.00320148
};
