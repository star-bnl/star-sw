TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 118055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53698; // +/- 1.66829e-05 cm/us All: East = -0.946742 +/- 0.0110608
  row.laserDriftVelocityWest	 =   5.53698; // +/- 1.66829e-05 cm/us All: West = 0.23033 +/- 0.00320641
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53698 +/- 1.66829e-05
  return (TDataSet *)tableSet;// West = 5.53639 +/- 1.74888e-05 East = 5.54296 +/- 5.5596e-05
};
