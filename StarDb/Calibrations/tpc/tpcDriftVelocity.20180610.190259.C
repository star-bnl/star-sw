TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 161029
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54846; // +/- 6.15392e-06 cm/us All: East = -0.265406 +/- 0.00195727
  row.laserDriftVelocityWest	 =   5.54846; // +/- 6.15392e-06 cm/us All: West = 0.271863 +/- 0.00130745
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54846 +/- 6.15392e-06
  return (TDataSet *)tableSet;// West = 5.54757 +/- 7.35231e-06 East = 5.55053 +/- 1.12463e-05
};
