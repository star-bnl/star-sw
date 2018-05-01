TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 54071
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.50897; // +/- 1.8783e-05 cm/us All: East = -0.0186501 +/- 0.00544405
  row.laserDriftVelocityWest	 =   5.50897; // +/- 1.8783e-05 cm/us All: West = 0.295944 +/- 0.004543
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.50897 +/- 1.8783e-05
  return (TDataSet *)tableSet;// West = 5.50842 +/- 2.48098e-05 East = 5.50972 +/- 2.87502e-05
};
