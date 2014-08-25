TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.DefaultTrackingParameters/DefaultTrackingParameters Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: TrackingParameters_st[0]--> TrackingParameters_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TrackingParameters")) return 0;
TrackingParameters_st row;
St_TrackingParameters *tableSet = new St_TrackingParameters("DefaultTrackingParameters",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.minSearch  =    0.1; // Minimum Search Window ;
    row.maxSearch  =    3.0; // Maximum Search Window ;
    row.scaling	   =    5.0; // Search Window Scaling ;
    row.maxChi2	   =   20.0; // Max Chi2 ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
