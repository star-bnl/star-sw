St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/sca/sca_filter_const Allocated rows: 1  Used rows: 1  Row size: 28 bytes
//  Table: sca_filter_const_st[0]--> sca_filter_const_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sca_filter_const")) return 0;
sca_filter_const_st row;
St_sca_filter_const *tableSet = new St_sca_filter_const("sca_filter_const",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.pointsPerTrack	 =         10; // ;
    row.minPT	 =          0; // transverse momentum         ;
    row.maxPT	 =          2; // ;
    row.minY	 =         -1; // rapidity                    ;
    row.maxY	 =          1; // ;
    row.InversSlope	 =        0.5; // inverse slope of mt spectra ;
    row.nGoodTraks	 =         50; // total no. of good tracks    ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
