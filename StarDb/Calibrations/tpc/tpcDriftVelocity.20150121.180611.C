TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 21030
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52417; // +/- 4.85919e-05 cm/us East: Slope = -4.21515 +/- 0.0086171 DV = 5.52417 +/- 4.85919e-05
  row.laserDriftVelocityWest	 =   5.5247; // +/- 3.2277e-05 cm/us West: Slope = -4.28471 +/- 0.00582982 DV = 5.5247 +/- 3.2277e-05
//row.scaleY                  	 = 9.97142e-07;// +/-2.11168e-09
  tableSet->AddAt(&row); 
  return (TDataSet *)tableSet; // 1e3*Delta: All = 5.52454 +/- 2.68861e-05
};
