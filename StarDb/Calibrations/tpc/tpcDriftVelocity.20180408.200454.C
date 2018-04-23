TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98038
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55031; // +/- 1.04359e-05 cm/us All: East = 0.00772431 +/- 0.00251158
  row.laserDriftVelocityWest	 =   5.55031; // +/- 1.04359e-05 cm/us All: West = 0.367436 +/- 0.0028449
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55031 +/- 1.04359e-05
  return (TDataSet *)tableSet;// West = 5.54919 +/- 1.56224e-05 East = 5.5512 +/- 1.40238e-05
};
