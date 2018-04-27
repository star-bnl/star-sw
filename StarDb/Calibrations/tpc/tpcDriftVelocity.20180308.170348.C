TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 67033
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53017; // +/- 6.42468e-05 cm/us All: East = -0.012258 +/- 0.126845
  row.laserDriftVelocityWest	 =   5.53017; // +/- 6.42468e-05 cm/us All: West = 0.254376 +/- 0.0344162
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53017 +/- 6.42468e-05
  return (TDataSet *)tableSet;// West = 5.52981 +/- 7.0391e-05 East = 5.53199 +/- 0.000157236
};
