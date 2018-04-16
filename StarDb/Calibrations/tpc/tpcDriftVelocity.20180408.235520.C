TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55047; // +/- 1.1889e-05 cm/us All: East = -5.19267 +/- 0.003005
  row.laserDriftVelocityWest	 =   5.55047; // +/- 1.1889e-05 cm/us All: West = -4.84209 +/- 0.0030668
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55047 +/- 1.1889e-05
  return (TDataSet *)tableSet;// West = 5.5495 +/- 1.70588e-05 East = 5.55139 +/- 1.65786e-05
};
