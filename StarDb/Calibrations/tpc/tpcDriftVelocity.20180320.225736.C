TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 79046
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56648; // +/- 8.68482e-06 cm/us All: East = -4.34899 +/- 0.0113764
  row.laserDriftVelocityWest	 =   5.56648; // +/- 8.68482e-06 cm/us All: West = -4.40093 +/- 0.00155349
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56648 +/- 8.68482e-06
  return (TDataSet *)tableSet;// West = 5.56649 +/- 8.78413e-06 East = 5.56601 +/- 5.79184e-05
};
