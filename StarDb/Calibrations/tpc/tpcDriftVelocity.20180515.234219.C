TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 135034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53746; // +/- 4.47361e-06 cm/us All: East = 0.145587 +/- 0.0018471
  row.laserDriftVelocityWest	 =   5.53746; // +/- 4.47361e-06 cm/us All: West = 0.173916 +/- 0.000879377
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53746 +/- 4.47361e-06
  return (TDataSet *)tableSet;// West = 5.53742 +/- 4.97199e-06 East = 5.53761 +/- 1.02516e-05
};
