TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 27045
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53195; // +/- 6.98225e-06 cm/us All: East = 0.130012 +/- 0.00268544
  row.laserDriftVelocityWest	 =   5.53195; // +/- 6.98225e-06 cm/us All: West = 0.577922 +/- 0.00142449
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53195 +/- 6.98225e-06
  return (TDataSet *)tableSet;// West = 5.53122 +/- 7.91852e-06 East = 5.5345 +/- 1.48025e-05
};
