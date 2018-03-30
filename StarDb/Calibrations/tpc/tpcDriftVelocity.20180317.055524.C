TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54723; // +/- 9.63582e-06 cm/us All: East = -1.32802 +/- 4.87387
  row.laserDriftVelocityWest	 =   5.54723; // +/- 9.63582e-06 cm/us All: West = -0.943718 +/- 0.0017229
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54723 +/- 9.63582e-06
  return (TDataSet *)tableSet;// West = 5.54723 +/- 9.63582e-06 East = 5.54716 +/- 0.0339476
};
