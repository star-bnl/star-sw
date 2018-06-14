TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 159046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55035; // +/- 1.46623e-05 cm/us All: East = -0.295293 +/- 0.0101628
  row.laserDriftVelocityWest	 =   5.55035; // +/- 1.46623e-05 cm/us All: West = 0.0938062 +/- 0.00286527
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55035 +/- 1.46623e-05
  return (TDataSet *)tableSet;// West = 5.55016 +/- 1.54718e-05 East = 5.55205 +/- 4.59316e-05
};
