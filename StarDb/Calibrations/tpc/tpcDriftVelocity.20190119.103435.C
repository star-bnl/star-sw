TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 19015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55684; // +/- 7.72205e-06 cm/us All: East = 0.0521134 +/- 0.00343373
  row.laserDriftVelocityWest	 =   5.55684; // +/- 7.72205e-06 cm/us All: West = 0.557239 +/- 0.00149318
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55684 +/- 7.72205e-06
  return (TDataSet *)tableSet;// West = 5.5564 +/- 8.43536e-06 East = 5.55915 +/- 1.91872e-05
};
