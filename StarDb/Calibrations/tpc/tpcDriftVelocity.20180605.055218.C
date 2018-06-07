TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 156005
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55811; // +/- 1.53748e-05 cm/us All: East = -0.0939765 +/- 0.00282746
  row.laserDriftVelocityWest	 =   5.55811; // +/- 1.53748e-05 cm/us All: West = 0.568892 +/- 0.0095403
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55811 +/- 1.53748e-05
  return (TDataSet *)tableSet;// West = 5.55474 +/- 5.32088e-05 East = 5.55842 +/- 1.60599e-05
};
