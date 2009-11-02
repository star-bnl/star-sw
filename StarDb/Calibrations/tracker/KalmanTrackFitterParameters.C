TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.KalmanTrackFitterParameters/KalmanTrackFitterParameters Allocated rows: 1  Used rows: 1  Row size: 8 bytes
//  Table: KalmanTrackFitterParameters_st[0]--> KalmanTrackFitterParameters_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_KalmanTrackFitterParameters")) return 0;
KalmanTrackFitterParameters_st row;
St_KalmanTrackFitterParameters *tableSet = new St_KalmanTrackFitterParameters("KalmanTrackFitterParameters",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.maxChi2	 	=         30.0; // Maximum Chi2 ;
    row.maxChi2Vtx	=        900.0; // Maximum Chi2 for vertex ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
