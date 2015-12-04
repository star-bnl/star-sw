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
  Int_t nPadsInner[13]  = {    88,    96,   104,   112,   118,   126,   134,   142,   150,   158,   166,   174,   182};
  Double_t RadInner[13] = {    60,  64.8,  69.6,  74.4,  79.2,    84,  88.8,  93.6,  98.8,   104, 109.2, 114.4, 119.6};
  
  Int_t nPadsOuter[32]  = {    98 ,    100,    102,    104,    106,    106,    108,    110,    112,    112,
			       114,    116,    118,    120,    122,    122,    124,    126,    128,    128,
			       130,    132,    134,    136,    138,    138,    140,    142,    144,    144,    
			       144,    144};
  memset(&row,0,tableSet->GetRowSize());
  row.ioSectorSeparation	 =      7.595; // ;
  row.firstRowWidth	 =     29.145; // ;
  row.lastRowWidth	 =      95.81; // ;
  row.innerPadRows	 =         13; // ;
  row.innerPadRows48	 =          8; // ;
  row.innerPadRows52	 =          5; // ;
  row.superInnerPadRows	 =          3; // ;
  row.innerSectorPadWidth	 =      0.285; // ;
  row.innerSectorPadLength	 =       1.15; // ;
  row.innerSectorPadPitch	 =      0.335; // ;
  row.innerSectorRowPitch1	 =        4.8; //*8 ;
  row.innerSectorRowPitch2	 =        5.2; //*5 ;
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
