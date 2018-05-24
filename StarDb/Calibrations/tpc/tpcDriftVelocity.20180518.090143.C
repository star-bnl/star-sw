TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 138015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54453; // +/- 6.19601e-06 cm/us All: East = -0.451708 +/- 0.00896043
  row.laserDriftVelocityWest	 =   5.54453; // +/- 6.19601e-06 cm/us All: West = -0.272173 +/- 0.00110737
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54453 +/- 6.19601e-06
  return (TDataSet *)tableSet;// West = 5.54452 +/- 6.25846e-06 East = 5.5454 +/- 4.39703e-05
};
