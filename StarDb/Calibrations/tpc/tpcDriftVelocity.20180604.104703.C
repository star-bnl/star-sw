TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 155020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55508; // +/- 7.23024e-06 cm/us All: East = -0.586858 +/- 0.00298963
  row.laserDriftVelocityWest	 =   5.55508; // +/- 7.23024e-06 cm/us All: West = 0.0356066 +/- 0.00142118
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55508 +/- 7.23024e-06
  return (TDataSet *)tableSet;// West = 5.55449 +/- 7.93497e-06 East = 5.55793 +/- 1.75493e-05
};
