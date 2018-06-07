TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55279; // +/- 5.99266e-06 cm/us All: East = 0.197246 +/- 0.00262803
  row.laserDriftVelocityWest	 =   5.55279; // +/- 5.99266e-06 cm/us All: West = 0.159665 +/- 0.00116808
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55279 +/- 5.99266e-06
  return (TDataSet *)tableSet;// West = 5.55284 +/- 6.57615e-06 East = 5.55258 +/- 1.45521e-05
};
