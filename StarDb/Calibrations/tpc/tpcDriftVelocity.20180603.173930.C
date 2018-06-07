TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 154031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55349; // +/- 6.33241e-06 cm/us All: East = -0.392283 +/- 0.00297368
  row.laserDriftVelocityWest	 =   5.55349; // +/- 6.33241e-06 cm/us All: West = 0.117189 +/- 0.00120698
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55349 +/- 6.33241e-06
  return (TDataSet *)tableSet;// West = 5.55314 +/- 6.76815e-06 East = 5.55595 +/- 1.79382e-05
};
