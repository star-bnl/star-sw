TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52819; // +/- 8.53949e-06 cm/us All: East = 1.49112 +/- 0.00877725
  row.laserDriftVelocityWest	 =   5.52819; // +/- 8.53949e-06 cm/us All: West = 2.52587 +/- 0.00155243
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52819 +/- 8.53949e-06
  return (TDataSet *)tableSet;// West = 5.52802 +/- 8.66687e-06 East = 5.53366 +/- 4.99928e-05
};
