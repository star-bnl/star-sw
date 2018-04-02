TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52706; // +/- 1.12634e-05 cm/us All: East = -2.86359 +/- 0.0104368
  row.laserDriftVelocityWest	 =   5.52706; // +/- 1.12634e-05 cm/us All: West = -1.90175 +/- 0.00205098
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52706 +/- 1.12634e-05
  return (TDataSet *)tableSet;// West = 5.52687 +/- 1.14697e-05 East = 5.53214 +/- 5.9656e-05
};
