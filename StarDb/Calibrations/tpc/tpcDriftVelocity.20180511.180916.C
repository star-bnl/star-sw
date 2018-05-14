TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54964; // +/- 1.08269e-05 cm/us All: East = 0.133997 +/- 0.0038463
  row.laserDriftVelocityWest	 =   5.54964; // +/- 1.08269e-05 cm/us All: West = 0.149386 +/- 0.00228497
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54964 +/- 1.08269e-05
  return (TDataSet *)tableSet;// West = 5.54961 +/- 1.26622e-05 East = 5.54971 +/- 2.08802e-05
};
