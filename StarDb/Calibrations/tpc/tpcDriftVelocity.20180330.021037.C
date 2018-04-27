TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 88066
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51706; // +/- 1.27681e-05 cm/us All: East = -0.720085 +/- 0.0088012
  row.laserDriftVelocityWest	 =   5.51706; // +/- 1.27681e-05 cm/us All: West = 0.226306 +/- 0.00238219
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51706 +/- 1.27681e-05
  return (TDataSet *)tableSet;// West = 5.5167 +/- 1.32326e-05 East = 5.52196 +/- 4.86147e-05
};
