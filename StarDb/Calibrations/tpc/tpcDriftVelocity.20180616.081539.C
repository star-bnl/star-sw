TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 167008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54737; // +/- 6.04649e-06 cm/us All: East = -0.479732 +/- 0.00284622
  row.laserDriftVelocityWest	 =   5.54737; // +/- 6.04649e-06 cm/us All: West = 0.156706 +/- 0.00116136
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54737 +/- 6.04649e-06
  return (TDataSet *)tableSet;// West = 5.5469 +/- 6.51144e-06 East = 5.55036 +/- 1.62936e-05
};
