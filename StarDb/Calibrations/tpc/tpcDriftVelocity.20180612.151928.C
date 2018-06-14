TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 163025
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54787; // +/- 7.09903e-06 cm/us All: East = -0.23008 +/- 0.00362762
  row.laserDriftVelocityWest	 =   5.54787; // +/- 7.09903e-06 cm/us All: West = 0.232905 +/- 0.00136428
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54787 +/- 7.09903e-06
  return (TDataSet *)tableSet;// West = 5.54757 +/- 7.5861e-06 East = 5.55 +/- 2.01364e-05
};
