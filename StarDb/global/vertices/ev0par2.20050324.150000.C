TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// ev0par2 Allocated rows: 6  Used rows: 6  Row size: 32 bytes
//  Table: ev0_ev0par2_st[0]--> ev0_ev0par2_st[2]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ev0_ev0par2")) return 0;
  ev0_ev0par2_st row;
  St_ev0_ev0par2 *tableSet = new St_ev0_ev0par2("ev0par2",6);
  int size = tableSet->GetRowSize();
//
//
// **************************************************************************
// First three rows are "loose" V0 cuts which allow for secondary V0s (in Xis).
// Second three rows are "tight" V0 cuts to restrict analyses to primary V0s.
// Within each set of three rows, the first row is for TPC-only cuts,
// the second row is for SVT-only cuts, and the third row is for SVT+TPC cuts.
// **************************************************************************
//
//
// ********************** Primary + Secondary V0 cuts ***********************
//
// TPC-only cuts
  memset(&row,0,size);
  row.dca	 =        1.2; // cut on dca between the two tracks ;
  row.dcav0	 =      10000; // cut on dca(impact param) of V0 from event vertex ;
  row.dlen	 =          2; // cut on dist. of decay from prim. vertex ;
  row.alpha_max	 =        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
  row.ptarm_max	 =        0.3; // Max. value of arm. pt allowed, only first entry used;
  row.dcapnmin	 =        0.0; // Min. value of tracks at interaction ;
  row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
  row.n_point	 =         11; // Min. number of points on track ;
  tableSet->AddAt(&row);
//
// SVT-only cuts
  memset(&row,0,size);
  row.dca	 =        1.2; // cut on dca between the two tracks ;
  row.dcav0	 =      10000; // cut on dca(impact param) of V0 from event vertex ;
  row.dlen	 =      10000; // cut on dist. of decay from prim. vertex ;
  row.alpha_max	 =        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
  row.ptarm_max	 =        0.3; // Max. value of arm. pt allowed, only first entry used;
  row.dcapnmin	 =        0.0; // Min. value of tracks at interaction ;
  row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
  row.n_point	 =          1; // Min. number of points on track ;
  tableSet->AddAt(&row);
//
// SVT+TPC cuts
  memset(&row,0,size);
  row.dca	 =        1.2; // cut on dca between the two tracks ;
  row.dcav0	 =      10000; // cut on dca(impact param) of V0 from event vertex ;
  row.dlen	 =        0.6; // cut on dist. of decay from prim. vertex ;
  row.alpha_max	 =        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
  row.ptarm_max	 =        0.3; // Max. value of arm. pt allowed, only first entry used;
  row.dcapnmin	 =        0.0; // Min. value of tracks at interaction ;
  row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
  row.n_point	 =         11; // Min. number of points on track ;
  tableSet->AddAt(&row);
//
// **************************** Primary V0 cuts *****************************
//
// TPC-only cuts
  memset(&row,0,size);
  row.dca	 =        1.2; // cut on dca between the two tracks ;
  row.dcav0	 =      10000; // cut on dca(impact param) of V0 from event vertex ;
  row.dlen	 =          2; // cut on dist. of decay from prim. vertex ;
  row.alpha_max	 =        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
  row.ptarm_max	 =        0.3; // Max. value of arm. pt allowed, only first entry used;
  row.dcapnmin	 =        0.0; // Min. value of tracks at interaction ;
  row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
  row.n_point	 =         11; // Min. number of points on track ;
  tableSet->AddAt(&row);
//
// SVT-only cuts
  memset(&row,0,size);
  row.dca	 =        1.2; // cut on dca between the two tracks ;
  row.dcav0	 =      10000; // cut on dca(impact param) of V0 from event vertex ;
  row.dlen	 =      10000; // cut on dist. of decay from prim. vertex ;
  row.alpha_max	 =        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
  row.ptarm_max	 =        0.3; // Max. value of arm. pt allowed, only first entry used;
  row.dcapnmin	 =        0.0; // Min. value of tracks at interaction ;
  row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
  row.n_point	 =          1; // Min. number of points on track ;
  tableSet->AddAt(&row);
//
// SVT+TPC cuts
  memset(&row,0,size);
  row.dca	 =        1.2; // cut on dca between the two tracks ;
  row.dcav0	 =      10000; // cut on dca(impact param) of V0 from event vertex ;
  row.dlen	 =        0.6; // cut on dist. of decay from prim. vertex ;
  row.alpha_max	 =        1.2; // Max. abs. value of arm. alpha allowed, only first entry used ;
  row.ptarm_max	 =        0.3; // Max. value of arm. pt allowed, only first entry used;
  row.dcapnmin	 =        0.0; // Min. value of tracks at interaction ;
  row.iflag	 =          0; // Controls execution flow, i.e. evaluate done now or not. ;
  row.n_point	 =         11; // Min. number of points on track ;
  tableSet->AddAt(&row);
//
// ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
