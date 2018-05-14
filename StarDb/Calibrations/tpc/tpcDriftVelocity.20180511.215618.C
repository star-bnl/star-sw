TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54892; // +/- 5.38973e-06 cm/us All: East = 0.130297 +/- 0.00176321
  row.laserDriftVelocityWest	 =   5.54892; // +/- 5.38973e-06 cm/us All: West = 0.171427 +/- 0.00114117
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54892 +/- 5.38973e-06
  return (TDataSet *)tableSet;// West = 5.54884 +/- 6.44503e-06 East = 5.54911 +/- 9.82935e-06
};
