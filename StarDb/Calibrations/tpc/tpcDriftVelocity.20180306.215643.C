TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 65054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53096; // +/- 9.11198e-06 cm/us All: East = -0.326893 +/- 0.00198891
  row.laserDriftVelocityWest	 =   5.53096; // +/- 9.11198e-06 cm/us All: West = 0.321501 +/- 0.00245999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53096 +/- 9.11198e-06
  return (TDataSet *)tableSet;// West = 5.52916 +/- 1.35019e-05 East = 5.53247 +/- 1.23479e-05
};
