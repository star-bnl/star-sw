TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 54032
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50922; // +/- 5.35465e-05 cm/us All: East = 0.188446 +/- 0.0296191
  row.laserDriftVelocityWest	 =   5.50922; // +/- 5.35465e-05 cm/us All: West = 0.160428 +/- 0.0194062
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50922 +/- 5.35465e-05
  return (TDataSet *)tableSet;// West = 5.50924 +/- 6.55634e-05 East = 5.50917 +/- 9.27947e-05
};
