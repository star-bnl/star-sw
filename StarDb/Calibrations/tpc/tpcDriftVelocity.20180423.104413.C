TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 113015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54056; // +/- 7.71144e-06 cm/us All: East = -0.140661 +/- 0.00283672
  row.laserDriftVelocityWest	 =   5.54056; // +/- 7.71144e-06 cm/us All: West = 0.198938 +/- 0.00159924
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54056 +/- 7.71144e-06
  return (TDataSet *)tableSet;// West = 5.54012 +/- 8.85136e-06 East = 5.54196 +/- 1.57088e-05
};
