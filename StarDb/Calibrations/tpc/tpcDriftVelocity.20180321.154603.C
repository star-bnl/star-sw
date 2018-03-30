TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 80065
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54814; // +/- 1.13156e-05 cm/us All: East = -1.77211 +/- 0.00741993
  row.laserDriftVelocityWest	 =   5.54814; // +/- 1.13156e-05 cm/us All: West = -1.05209 +/- 0.00217786
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54814 +/- 1.13156e-05
  return (TDataSet *)tableSet;// West = 5.54781 +/- 1.1838e-05 East = 5.55164 +/- 3.8514e-05
};
