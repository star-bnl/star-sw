TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100098
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5216; // +/- 4.80122e-05 cm/us East: Slope = 0.33386 +/- 0.00966467 DV = 5.5216 +/- 4.80122e-05
  row.laserDriftVelocityWest	 =   5.5216; // +/- 4.80122e-05 cm/us West: Slope = 0.33386 +/- 0.00966467 DV = 5.5216 +/- 4.80122e-05
//row.scaleY                  	 = 4.67766e-07;// +/-3.77897e-09
  tableSet->AddAt(&row); 
  return (TDataSet *)tableSet; // 1e3*Delta: All = 5.5216 +/- 4.80122e-05
};
