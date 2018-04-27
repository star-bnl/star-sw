TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 108009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54513; // +/- 1.75052e-05 cm/us All: East = -0.269733 +/- 0.00367936
  row.laserDriftVelocityWest	 =   5.54513; // +/- 1.75052e-05 cm/us All: West = 0.151628 +/- 0.00635251
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54513 +/- 1.75052e-05
  return (TDataSet *)tableSet;// West = 5.54347 +/- 3.45518e-05 East = 5.5457 +/- 2.0304e-05
};
