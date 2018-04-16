TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 94043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52035; // +/- 8.87302e-06 cm/us All: East = 1.35554 +/- 0.00337959
  row.laserDriftVelocityWest	 =   5.52035; // +/- 8.87302e-06 cm/us All: West = 1.62763 +/- 0.00179017
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52035 +/- 8.87302e-06
  return (TDataSet *)tableSet;// West = 5.52002 +/- 1.00433e-05 East = 5.52151 +/- 1.89405e-05
};
