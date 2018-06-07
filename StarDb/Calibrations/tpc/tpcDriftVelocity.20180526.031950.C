TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145048
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55142; // +/- 0.000113795 cm/us All: East = 0.552365 +/- 2.14584
  row.laserDriftVelocityWest	 =   5.55142; // +/- 0.000113795 cm/us All: West = 0.24367 +/- 0.121975
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55142 +/- 0.000113795
  return (TDataSet *)tableSet;// West = 5.55141 +/- 0.000114034 East = 5.5536 +/- 0.00175695
};
