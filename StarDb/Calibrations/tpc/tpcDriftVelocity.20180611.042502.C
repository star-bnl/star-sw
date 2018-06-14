TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5477; // +/- 1.95597e-05 cm/us All: East = -0.378203 +/- 0.016867
  row.laserDriftVelocityWest	 =   5.5477; // +/- 1.95597e-05 cm/us All: West = 0.209941 +/- 0.00363033
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5477 +/- 1.95597e-05
  return (TDataSet *)tableSet;// West = 5.54754 +/- 2.00697e-05 East = 5.55072 +/- 8.73165e-05
};
