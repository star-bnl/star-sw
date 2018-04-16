TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97063
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54682; // +/- 8.0183e-06 cm/us All: East = -4.40071 +/- 0.00178915
  row.laserDriftVelocityWest	 =   5.54682; // +/- 8.0183e-06 cm/us All: West = -4.29106 +/- 0.00234932
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54682 +/- 8.0183e-06
  return (TDataSet *)tableSet;// West = 5.54642 +/- 1.33077e-05 East = 5.54704 +/- 1.00468e-05
};
