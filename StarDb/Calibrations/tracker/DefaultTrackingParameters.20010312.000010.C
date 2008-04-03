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
    row.minSearch  =    1.5*10; // Minimum Search Window ;
    row.maxSearch  =    8.0*10; // Maximum Search Window ;
    row.scaling	   =    3.0*10; // Search Window Scaling ;
    row.maxChi2	   =   20.0; // Max Chi2 ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
