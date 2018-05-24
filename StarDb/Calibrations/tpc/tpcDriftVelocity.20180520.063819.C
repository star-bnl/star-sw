TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 140007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55874; // +/- 5.92834e-06 cm/us All: East = 0.350543 +/- 0.00461168
  row.laserDriftVelocityWest	 =   5.55874; // +/- 5.92834e-06 cm/us All: West = 0.202019 +/- 0.00107318
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55874 +/- 5.92834e-06
  return (TDataSet *)tableSet;// West = 5.55879 +/- 6.09827e-06 East = 5.55785 +/- 2.52891e-05
};
