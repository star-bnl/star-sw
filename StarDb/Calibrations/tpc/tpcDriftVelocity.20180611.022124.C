TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 161049
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5487; // +/- 6.06213e-06 cm/us All: East = -0.371385 +/- 0.00165739
  row.laserDriftVelocityWest	 =   5.5487; // +/- 6.06213e-06 cm/us All: West = 0.292942 +/- 0.00141262
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5487 +/- 6.06213e-06
  return (TDataSet *)tableSet;// West = 5.54717 +/- 7.93193e-06 East = 5.55084 +/- 9.40013e-06
};
