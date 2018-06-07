TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 151035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55031; // +/- 1.8847e-05 cm/us All: East = -0.3329 +/- 0.00804173
  row.laserDriftVelocityWest	 =   5.55031; // +/- 1.8847e-05 cm/us All: West = 0.178919 +/- 0.00376289
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55031 +/- 1.8847e-05
  return (TDataSet *)tableSet;// West = 5.54976 +/- 2.11561e-05 East = 5.55243 +/- 4.14868e-05
};
