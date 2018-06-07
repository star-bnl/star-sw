TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55064; // +/- 5.12509e-06 cm/us All: East = 0.050725 +/- 0.00372132
  row.laserDriftVelocityWest	 =   5.55064; // +/- 5.12509e-06 cm/us All: West = 0.171703 +/- 0.000941785
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55064 +/- 5.12509e-06
  return (TDataSet *)tableSet;// West = 5.55059 +/- 5.29208e-06 East = 5.55129 +/- 2.05642e-05
};
