TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 96045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54444; // +/- 1.4571e-05 cm/us All: East = -0.0884723 +/- 0.00352601
  row.laserDriftVelocityWest	 =   5.54444; // +/- 1.4571e-05 cm/us All: West = 0.492736 +/- 0.0039406
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54444 +/- 1.4571e-05
  return (TDataSet *)tableSet;// West = 5.54269 +/- 2.16621e-05 East = 5.54588 +/- 1.96915e-05
};
