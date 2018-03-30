TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 68054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56629; // +/- 4.83919e-05 cm/us All: East = -4.99083 +/- 0.0575398
  row.laserDriftVelocityWest	 =   5.56629; // +/- 4.83919e-05 cm/us All: West = -4.2923 +/- 0.0149582
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56629 +/- 4.83919e-05
  return (TDataSet *)tableSet;// West = 5.5658 +/- 5.17701e-05 East = 5.56974 +/- 0.000136195
};
