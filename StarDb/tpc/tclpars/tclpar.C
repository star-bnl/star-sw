St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/tclpars/tclpar Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: tcl_tclpar_st[0]--> tcl_tclpar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tcl_tclpar")) return 0;
tcl_tclpar_st row;
St_tcl_tclpar *tableSet = new St_tcl_tclpar("tclpar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.decon	 =          1; // deconvolution method (0=no decon) ;
    row.mc	 =          1; // look at and fill (or not) MC data (1/0) ;
    row.mf_cpad	 =          2; // closest sep. in pad for Mountain Finder ;
    row.mf_ctim	 =          2; // closest sep. in tdc in Mountain Finder ;
    row.mf_min	 =         20; // minimum peak height in Mountain Finder ;
    row.min_nseq	 =          2; // min # sequences for a cluster to be hit ;
    row.tfit	 =          0; // method used to fit time (0=mean) ;
    row.triage	 =          0; // use cluster cuts to see if >1 peak poss. ;
    row.dxy_fact	 =          1; // fudge factor for uncertainty in x and y ;
    row.dz_fact	 =          1; // fudge factor for uncertainty in z ;
    row.mf_pv	 =         10; // peak:valley cut for Mountain Finder ;
    row.triage_rmscut	 =          3; // cut on rms_pad + rms_tdc ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
