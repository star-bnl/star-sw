TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 99035
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55333; // +/- 1.73507e-05 cm/us All: East = 0.0146288 +/- 0.00336251
  row.laserDriftVelocityWest	 =   5.55333; // +/- 1.73507e-05 cm/us All: West = 0.776592 +/- 0.00886831
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55333 +/- 1.73507e-05
  return (TDataSet *)tableSet;// West = 5.54965 +/- 4.3496e-05 East = 5.55402 +/- 1.89213e-05
};
