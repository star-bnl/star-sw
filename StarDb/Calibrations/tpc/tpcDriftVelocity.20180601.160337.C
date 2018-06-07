TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 152057
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55087; // +/- 5.98997e-06 cm/us All: East = 0.289379 +/- 0.00330873
  row.laserDriftVelocityWest	 =   5.55087; // +/- 5.98997e-06 cm/us All: West = 0.135746 +/- 0.00115884
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55087 +/- 5.98997e-06
  return (TDataSet *)tableSet;// West = 5.55097 +/- 6.33898e-06 East = 5.55006 +/- 1.83045e-05
};
