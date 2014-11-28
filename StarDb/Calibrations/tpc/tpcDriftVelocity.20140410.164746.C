TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =    5.52327; //5.52332; // +/- 1.87661e-05 cm/us East: Slope = 0.116232 +/- 0.00342479 DV = 5.52332 +/- 1.87661e-05
  row.laserDriftVelocityWest	 =    5.52327; //5.52317; // +/- 2.82866e-05 cm/us West: Slope = 0.174205 +/- 0.00269996 DV = 5.52317 +/- 2.82866e-05
//row.scaleY                  	 = 2.30323e-07;// +/-2.06381e-09
  tableSet->AddAt(&row); 
  return (TDataSet *)tableSet; // 1e3*Delta: All = 5.52327 +/- 1.56377e-05
};
