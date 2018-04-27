TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91095
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5185; // +/- 1.30094e-05 cm/us All: East = -0.572345 +/- 0.00716586
  row.laserDriftVelocityWest	 =   5.5185; // +/- 1.30094e-05 cm/us All: West = 0.245422 +/- 0.00245147
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5185 +/- 1.30094e-05
  return (TDataSet *)tableSet;// West = 5.51806 +/- 1.37063e-05 East = 5.52254 +/- 4.13237e-05
};
