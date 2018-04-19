TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 108009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55536; // +/- 1.89915e-05 cm/us All: East = 0.114339 +/- 0.00420436
  row.laserDriftVelocityWest	 =   5.55536; // +/- 1.89915e-05 cm/us All: West = 0.577933 +/- 0.00695622
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55536 +/- 1.89915e-05
  return (TDataSet *)tableSet;// West = 5.55351 +/- 3.71718e-05 East = 5.55602 +/- 2.20926e-05
};
