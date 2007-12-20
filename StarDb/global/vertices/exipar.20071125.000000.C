TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// exipar Allocated rows: 3  Used rows: 3  Row size: 32 bytes
//  Table: exi_exipar_st[0]--> exi_exipar_st[2]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_exi_exipar")) return 0;
  exi_exipar_st row;
  St_exi_exipar *tableSet = new St_exi_exipar("exipar",3);
  int size = tableSet->GetRowSize();
//
//
// **************************************************************************
// There is one set of three rows: the first row is for TPC-only cuts,
// the second row is for SVT-only cuts, and the third row is for SVT+TPC cuts.
// **************************************************************************
//
//
// TPC-only cuts
  memset(&row,0,size);
  row.use_pid	 =          0; // logical flag to control usage of global pid ;
  row.dca_max	 =        1.2; // cut on dca between the two tracks ;
  row.bxi_max	 =      10000; // cut on impact param. of xi from prim. vertex ;
  row.rv_xi	 =          2; // cut on min. dist. of decay from prim. vertex ;
  row.rv_v0	 =          4; // cut on min. dist. of decay from prim. vertex ;
  row.dmass	 =       0.01; // v0 mass cut +/- [dmass] ;
  row.bpn_v0	 =        0.0; // cut on v0 pion daught. impact param. ;
  row.pchisq	 =          0; // cut on chi^2 probability of vertex fit;
  tableSet->AddAt(&row);
//
// SVT-only cuts
  memset(&row,0,size);
  row.use_pid	 =          0; // logical flag to control usage of global pid ;
  row.dca_max	 =          0; // cut on dca between the two tracks ;
  row.bxi_max	 =          0; // cut on impact param. of xi from prim. vertex ;
  row.rv_xi	 =        999; // cut on min. dist. of decay from prim. vertex ;
  row.rv_v0	 =        999; // cut on min. dist. of decay from prim. vertex ;
  row.dmass	 =          0; // v0 mass cut +/- [dmass] ;
  row.bpn_v0	 =        999; // cut on v0 pion daught. impact param. ;
  row.pchisq	 =          0; // cut on chi^2 probability of vertex fit;
  tableSet->AddAt(&row);
//
// SVT+TPC cuts
  memset(&row,0,size);
  row.use_pid	 =          0; // logical flag to control usage of global pid ;
  row.dca_max	 =        1.2; // cut on dca between the two tracks ;
  row.bxi_max	 =      10000; // cut on impact param. of xi from prim. vertex ;
  row.rv_xi	 =          2; // cut on min. dist. of decay from prim. vertex ;
  row.rv_v0	 =          4; // cut on min. dist. of decay from prim. vertex ;
  row.dmass	 =       0.01; // v0 mass cut +/- [dmass] ;
  row.bpn_v0	 =        0.0; // cut on v0 pion daught. impact param. ;
  row.pchisq	 =          0; // cut on chi^2 probability of vertex fit;
  tableSet->AddAt(&row);
//
// ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
