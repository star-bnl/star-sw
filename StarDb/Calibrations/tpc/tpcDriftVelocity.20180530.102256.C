TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 150017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5493; // +/- 6.96959e-06 cm/us All: East = 0.0242817 +/- 0.00403204
  row.laserDriftVelocityWest	 =   5.5493; // +/- 6.96959e-06 cm/us All: West = 0.35934 +/- 0.00130121
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5493 +/- 6.96959e-06
  return (TDataSet *)tableSet;// West = 5.54912 +/- 7.34009e-06 East = 5.55093 +/- 2.22178e-05
};
