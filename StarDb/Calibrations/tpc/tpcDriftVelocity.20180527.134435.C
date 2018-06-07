TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 147022
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55015; // +/- 7.70048e-05 cm/us All: East = 0.376867 +/- 0.103102
  row.laserDriftVelocityWest	 =   5.55015; // +/- 7.70048e-05 cm/us All: West = 0.424595 +/- 0.0634072
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55015 +/- 7.70048e-05
  return (TDataSet *)tableSet;// West = 5.54966 +/- 0.000121712 East = 5.55048 +/- 9.94364e-05
};
