TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 124006
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52463; // +/- 1.30395e-05 cm/us All: East = -1.00123 +/- 0.00413484
  row.laserDriftVelocityWest	 =   5.52463; // +/- 1.30395e-05 cm/us All: West = -0.249566 +/- 0.00284832
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52463 +/- 1.30395e-05
  return (TDataSet *)tableSet;// West = 5.52335 +/- 1.59987e-05 East = 5.52717 +/- 2.25044e-05
};
