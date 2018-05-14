TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 128034
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55983; // +/- 1.22609e-05 cm/us All: East = -0.142669 +/- 0.0064839
  row.laserDriftVelocityWest	 =   5.55983; // +/- 1.22609e-05 cm/us All: West = 0.642134 +/- 0.00232016
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55983 +/- 1.22609e-05
  return (TDataSet *)tableSet;// West = 5.55934 +/- 1.30183e-05 East = 5.5637 +/- 3.64795e-05
};
