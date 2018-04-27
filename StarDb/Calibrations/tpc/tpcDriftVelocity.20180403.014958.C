TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 92093
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51934; // +/- 9.79628e-06 cm/us All: East = -0.615301 +/- 0.00584134
  row.laserDriftVelocityWest	 =   5.51934; // +/- 9.79628e-06 cm/us All: West = 0.21754 +/- 0.0018289
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51934 +/- 9.79628e-06
  return (TDataSet *)tableSet;// West = 5.51894 +/- 1.02491e-05 East = 5.52351 +/- 3.33249e-05
};
