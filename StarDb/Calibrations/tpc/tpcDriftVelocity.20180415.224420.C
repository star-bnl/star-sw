TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 105043
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55407; // +/- 5.22948e-05 cm/us All: East = -5.89523 +/- 0.0561783
  row.laserDriftVelocityWest	 =   5.55407; // +/- 5.22948e-05 cm/us All: West = -5.5191 +/- 0.0330966
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55407 +/- 5.22948e-05
  return (TDataSet *)tableSet;// West = 5.55341 +/- 7.15117e-05 East = 5.55482 +/- 7.66695e-05
};
