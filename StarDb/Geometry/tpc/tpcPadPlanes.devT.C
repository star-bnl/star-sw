TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Geometry/tpc/.tpcPadPlanes/tpcPadPlanes Allocated rows: 1  Used rows: 1  Row size: 1392 bytes
  //  Table: tpcPadPlanes_st[0]--> tpcPadPlanes_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcPadPlanes")) return 0;
  tpcPadPlanes_st row;
  St_tpcPadPlanes *tableSet = new St_tpcPadPlanes("tpcPadPlanes",1);
  //
#if 0
  Int_t nPadsInner[13]  = {    88,    96,   104,   112,   118,   126,   134,   142,   150,   158,   166,   174,   182};
  Double_t RadInner[13] = {    60,  64.8,  69.6,  74.4,  79.2,    84,  88.8,  93.6,  98.8,   104, 109.2, 114.4, 119.6};
#else
//   Int_t nPadsInner[32]  = {    44,    46,    48,    50,    52,    52,    54,    56,    58,    60,
// 			       60,    62,    64,    66,    68,    68,    70,    72,    74,    76,
// 			       76,    78,    80,    82,    84,    84,    86,    88,    90,    92,
// 			       92,    94};
// from StarVMC/Geometry/TpcxGeo/TpcxGeo1.xml
  Int_t nPadsInner[32]  = {42,44,46,48,50,50,52,54,56,58,
			   58,60,62,64,66,66,68,70,72,74,
			   74,76,78,80,82,82,84,86,88,90,
			   90,92};
  Double_t RadInner[32] = { 57.2, 59.2, 61.2, 63.2, 65.2, 67.2, 69.2, 71.2, 73.2, 75.2,
			    77.2, 79.2, 81.2, 83.2, 85.2, 87.2, 89.2, 91.2, 93.2, 95.2,
			    97.2, 99.2,101.2,103.2,105.2,107.2,109.2,111.2,113.2,115.2,
			    117.2,119.2};
  //  for (Int_t i = 0; i < 32; i++) RadInner[i] = 60 + 2*i;
#endif
  Int_t nPadsOuter[32]  = {    98 ,    100,    102,    104,    106,    106,    108,    110,    112,    112,
			       114,    116,    118,    120,    122,    122,    124,    126,    128,    128,
			       130,    132,    134,    136,    138,    138,    140,    142,    144,    144,    
			       144,    144};
  memset(&row,0,tableSet->GetRowSize());
  row.ioSectorSeparation	 =      7.595; // ;
  row.firstRowWidth	 =     29.145; // ;
  row.lastRowWidth	 =      95.81; // ;
  row.innerPadRows	 =         32; // ;
  row.innerPadRows48	 =          8; // ;
  row.innerPadRows52	 =          5; // ;
  row.superInnerPadRows	 =          3; // ;
  row.innerSectorPadWidth	 =      0.62; // 0.285;
  row.innerSectorPadLength	 =      1.95; //  1.15;
  row.innerSectorPadPitch	 =      0.67; // 0.335;
  row.innerSectorRowPitch1	 =      2.0;  //  4.8; 
  row.innerSectorRowPitch2	 =      2.0;  //  5.2; 
  row.innerSectorEdge	 =     51.905; // ;
  row.innerSectorPadPlaneZ	 =    209.707; // ;
  for (Int_t i = 0; i < row.innerPadRows; i++) {
    row.innerPadsPerRow[i] = nPadsInner[i];
    row.innerRowRadii[i]   = RadInner[i];
  }
  row.firstPadRow	 =  row.innerRowRadii[0];   
  row.outerPadRows	 =         32; // ;
  row.padRows	         = row.innerPadRows + row.outerPadRows;
  row.outerSectorPadWidth	 =       0.62; // ;
  row.outerSectorPadLength	 =       1.95; // ;
  row.outerSectorPadPitch	 =       0.67; // ;
  row.outerSectorLength	 =         62; // ;
  row.superOuterPadRows	 =          1; // ;
  row.firstOuterSectorPadRow	 =    127.195; // ;
  row.outerSectorRowPitch	 =          2; // ;
  row.outerSectorEdge	 =    121.732; // ;
  row.outerSectorPadPlaneZ	 =    210.107; // ;
  for (Int_t i = 0; i < row.outerPadRows; i++) {
    row.outerPadsPerRow[i] = nPadsOuter[i];
    row.outerRowRadii[i]   = row.firstOuterSectorPadRow + row.outerSectorRowPitch*i;
  }
  row.lastOuterSectorPadRow	 =  row.outerRowRadii[row.outerPadRows-1];   
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
