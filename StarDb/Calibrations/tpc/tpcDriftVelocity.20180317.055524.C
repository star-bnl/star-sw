TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54717; // +/- 9.8503e-06 cm/us All: East = 0.796853 +/- 4.02458
  row.laserDriftVelocityWest	 =   5.54717; // +/- 9.8503e-06 cm/us All: West = 0.201207 +/- 0.00176686
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54717 +/- 9.8503e-06
  return (TDataSet *)tableSet;// West = 5.54717 +/- 9.85041e-06 East = 5.5447 +/- 0.00208264
};
