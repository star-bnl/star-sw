TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156014
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55592; // +/- 2.46501e-05 cm/us All: East = 0.0868409 +/- 0.00493783
  row.laserDriftVelocityWest	 =   5.55592; // +/- 2.46501e-05 cm/us All: West = 0.393515 +/- 0.00374226
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55592 +/- 2.46501e-05
  return (TDataSet *)tableSet;// West = 5.55393 +/- 5.80969e-05 East = 5.55635 +/- 2.72218e-05
};
