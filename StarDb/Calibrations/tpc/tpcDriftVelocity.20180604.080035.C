TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 155011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55437; // +/- 7.34659e-06 cm/us All: East = -0.0660717 +/- 0.00456965
  row.laserDriftVelocityWest	 =   5.55437; // +/- 7.34659e-06 cm/us All: West = 0.248711 +/- 0.00135046
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55437 +/- 7.34659e-06
  return (TDataSet *)tableSet;// West = 5.55423 +/- 7.66108e-06 East = 5.55591 +/- 2.59069e-05
};
