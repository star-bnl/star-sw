TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52405; // +/- 7.69145e-06 cm/us All: East = 0.0463253 +/- 0.00553648
  row.laserDriftVelocityWest	 =   5.52405; // +/- 7.69145e-06 cm/us All: West = 0.349665 +/- 0.00141688
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52405 +/- 7.69145e-06
  return (TDataSet *)tableSet;// West = 5.52394 +/- 7.9422e-06 East = 5.52565 +/- 3.08535e-05
};
