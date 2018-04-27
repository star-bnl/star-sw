TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5182; // +/- 1.05815e-05 cm/us All: East = -0.448339 +/- 0.00660504
  row.laserDriftVelocityWest	 =   5.5182; // +/- 1.05815e-05 cm/us All: West = 0.19304 +/- 0.00197426
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5182 +/- 1.05815e-05
  return (TDataSet *)tableSet;// West = 5.51792 +/- 1.10294e-05 East = 5.52142 +/- 3.75125e-05
};
