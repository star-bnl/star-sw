TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162003
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54749; // +/- 2.95532e-05 cm/us All: East = -0.269827 +/- 0.0344549
  row.laserDriftVelocityWest	 =   5.54749; // +/- 2.95532e-05 cm/us All: West = 0.261449 +/- 0.00646114
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54749 +/- 2.95532e-05
  return (TDataSet *)tableSet;// West = 5.54722 +/- 3.08936e-05 East = 5.55039 +/- 0.00010143
};
