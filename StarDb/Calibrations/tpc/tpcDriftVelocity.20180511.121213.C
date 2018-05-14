TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 131027
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54938; // +/- 7.41715e-06 cm/us All: East = 0.293064 +/- 0.0025234
  row.laserDriftVelocityWest	 =   5.54938; // +/- 7.41715e-06 cm/us All: West = 0.34749 +/- 0.000998275
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54938 +/- 7.41715e-06
  return (TDataSet *)tableSet;// West = 5.5491 +/- 8.71923e-06 East = 5.55012 +/- 1.41089e-05
};
