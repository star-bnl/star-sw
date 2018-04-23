TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 74002
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51813; // +/- 9.8319e-06 cm/us All: East = 0.22476 +/- 0.00698694
  row.laserDriftVelocityWest	 =   5.51813; // +/- 9.8319e-06 cm/us All: West = 0.164827 +/- 0.00150665
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51813 +/- 9.8319e-06
  return (TDataSet *)tableSet;// West = 5.51819 +/- 1.01467e-05 East = 5.51723 +/- 3.97783e-05
};
