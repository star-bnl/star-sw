TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 132074
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54761; // +/- 8.18243e-06 cm/us All: East = -0.144745 +/- 0.00476618
  row.laserDriftVelocityWest	 =   5.54761; // +/- 8.18243e-06 cm/us All: West = 0.220885 +/- 0.00153075
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54761 +/- 8.18243e-06
  return (TDataSet *)tableSet;// West = 5.54741 +/- 8.6412e-06 East = 5.54938 +/- 2.54506e-05
};
