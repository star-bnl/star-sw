TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5503; // +/- 5.06542e-06 cm/us All: East = 0.27236 +/- 0.00194346
  row.laserDriftVelocityWest	 =   5.5503; // +/- 5.06542e-06 cm/us All: West = 0.208162 +/- 0.00100752
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5503 +/- 5.06542e-06
  return (TDataSet *)tableSet;// West = 5.5504 +/- 5.7033e-06 East = 5.54994 +/- 1.10227e-05
};
