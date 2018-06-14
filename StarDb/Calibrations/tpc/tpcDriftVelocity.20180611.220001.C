TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 162041
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54744; // +/- 1.01468e-05 cm/us All: East = -0.103064 +/- 0.00500846
  row.laserDriftVelocityWest	 =   5.54744; // +/- 1.01468e-05 cm/us All: West = 0.28263 +/- 0.00197071
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54744 +/- 1.01468e-05
  return (TDataSet *)tableSet;// West = 5.54714 +/- 1.09422e-05 East = 5.54927 +/- 2.71096e-05
};
