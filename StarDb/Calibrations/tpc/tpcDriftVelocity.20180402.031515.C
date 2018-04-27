TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91119
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51882; // +/- 1.33848e-05 cm/us All: East = -0.571982 +/- 0.00637316
  row.laserDriftVelocityWest	 =   5.51882; // +/- 1.33848e-05 cm/us All: West = 0.292851 +/- 0.00260279
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51882 +/- 1.33848e-05
  return (TDataSet *)tableSet;// West = 5.51814 +/- 1.44657e-05 East = 5.52285 +/- 3.52888e-05
};
