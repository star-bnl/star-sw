TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 133009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54771; // +/- 1.07334e-05 cm/us All: East = -0.410697 +/- 0.00793043
  row.laserDriftVelocityWest	 =   5.54771; // +/- 1.07334e-05 cm/us All: West = 0.374115 +/- 0.00198589
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54771 +/- 1.07334e-05
  return (TDataSet *)tableSet;// West = 5.54743 +/- 1.10962e-05 East = 5.55178 +/- 4.2324e-05
};
