TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52735; // +/- 6.67186e-05 cm/us All: East = -3.52355 +/- 53.2643
  row.laserDriftVelocityWest	 =   5.52735; // +/- 6.67186e-05 cm/us All: West = -1.91717 +/- 0.0512374
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52735 +/- 6.67186e-05
  return (TDataSet *)tableSet;// West = 5.52735 +/- 6.67186e-05 East = 5.55143 +/- 0.424519
};
