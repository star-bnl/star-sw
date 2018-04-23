TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91095
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52843; // +/- 1.34871e-05 cm/us All: East = -0.561274 +/- 0.0074224
  row.laserDriftVelocityWest	 =   5.52843; // +/- 1.34871e-05 cm/us All: West = 0.254561 +/- 0.00252908
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52843 +/- 1.34871e-05
  return (TDataSet *)tableSet;// West = 5.52801 +/- 1.41722e-05 East = 5.53246 +/- 4.39101e-05
};
