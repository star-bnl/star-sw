TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92093
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52929; // +/- 9.85237e-06 cm/us All: East = -0.567174 +/- 0.00574595
  row.laserDriftVelocityWest	 =   5.52929; // +/- 9.85237e-06 cm/us All: West = 0.24935 +/- 0.00184879
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52929 +/- 9.85237e-06
  return (TDataSet *)tableSet;// West = 5.52888 +/- 1.03235e-05 East = 5.53339 +/- 3.29904e-05
};
