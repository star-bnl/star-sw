TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 21001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55848; // +/- 9.26732e-06 cm/us All: East = -0.462934 +/- 0.00431852
  row.laserDriftVelocityWest	 =   5.55848; // +/- 9.26732e-06 cm/us All: West = -0.382902 +/- 0.00179361
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55848 +/- 9.26732e-06
  return (TDataSet *)tableSet;// West = 5.55846 +/- 9.99027e-06 East = 5.55862 +/- 2.48129e-05
};
