TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55042; // +/- 5.06705e-06 cm/us All: East = 0.0661386 +/- 0.00295468
  row.laserDriftVelocityWest	 =   5.55042; // +/- 5.06705e-06 cm/us All: West = 0.131374 +/- 0.000941214
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55042 +/- 5.06705e-06
  return (TDataSet *)tableSet;// West = 5.55039 +/- 5.33072e-06 East = 5.55071 +/- 1.63131e-05
};
