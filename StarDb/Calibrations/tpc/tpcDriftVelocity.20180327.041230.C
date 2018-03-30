TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 86001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52883; // +/- 7.73055e-06 cm/us All: East = 2.34733 +/- 0.00715656
  row.laserDriftVelocityWest	 =   5.52883; // +/- 7.73055e-06 cm/us All: West = 2.37602 +/- 0.00141237
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52883 +/- 7.73055e-06
  return (TDataSet *)tableSet;// West = 5.52885 +/- 7.88987e-06 East = 5.5284 +/- 3.86632e-05
};
