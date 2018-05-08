TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 123046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52112; // +/- 1.10812e-05 cm/us All: East = -0.75646 +/- 0.00485055
  row.laserDriftVelocityWest	 =   5.52112; // +/- 1.10812e-05 cm/us All: West = 0.351895 +/- 0.00222371
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52112 +/- 1.10812e-05
  return (TDataSet *)tableSet;// West = 5.52002 +/- 1.2313e-05 East = 5.5258 +/- 2.54173e-05
};
