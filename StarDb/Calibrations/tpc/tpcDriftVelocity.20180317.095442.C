TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53864; // +/- 1.11526e-05 cm/us All: East = -999 +/- 999
  row.laserDriftVelocityWest	 =   5.53864; // +/- 1.11526e-05 cm/us All: West = 2.05989 +/- 0.00200708
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53864 +/- 1.11526e-05
  return (TDataSet *)tableSet;// West = 5.53864 +/- 1.11526e-05 East = -999 +/- 999
};
