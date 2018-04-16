TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 95019
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5216; // +/- 1.37618e-05 cm/us All: East = -1.03913 +/- 0.00715187
  row.laserDriftVelocityWest	 =   5.5216; // +/- 1.37618e-05 cm/us All: West = 0.0741287 +/- 0.00261954
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5216 +/- 1.37618e-05
  return (TDataSet *)tableSet;// West = 5.52095 +/- 1.45763e-05 East = 5.52687 +/- 4.17523e-05
};
