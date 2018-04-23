TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55404; // +/- 7.15816e-06 cm/us All: East = -0.352714 +/- 0.00437745
  row.laserDriftVelocityWest	 =   5.55404; // +/- 7.15816e-06 cm/us All: West = 0.227716 +/- 0.00133003
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55404 +/- 7.15816e-06
  return (TDataSet *)tableSet;// West = 5.55378 +/- 7.48747e-06 East = 5.55683 +/- 2.44051e-05
};
