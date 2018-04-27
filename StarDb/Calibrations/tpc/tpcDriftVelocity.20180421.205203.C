TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 111039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54399; // +/- 7.1687e-06 cm/us All: East = -0.275756 +/- 0.00485498
  row.laserDriftVelocityWest	 =   5.54399; // +/- 7.1687e-06 cm/us All: West = 0.199684 +/- 0.00133015
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54399 +/- 7.1687e-06
  return (TDataSet *)tableSet;// West = 5.54381 +/- 7.44264e-06 East = 5.54624 +/- 2.66681e-05
};
