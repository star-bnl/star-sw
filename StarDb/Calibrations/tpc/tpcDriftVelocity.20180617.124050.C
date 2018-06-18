TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 168020
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54596; // +/- 5.98658e-06 cm/us All: East = 0.237782 +/- 0.00289762
  row.laserDriftVelocityWest	 =   5.54596; // +/- 5.98658e-06 cm/us All: West = 0.227652 +/- 0.00113231
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54596 +/- 5.98658e-06
  return (TDataSet *)tableSet;// West = 5.54598 +/- 6.39818e-06 East = 5.54583 +/- 1.69651e-05
};
