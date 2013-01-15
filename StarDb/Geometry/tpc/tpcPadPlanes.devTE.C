TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Geometry/tpc/.tpcPadPlanes/tpcPadPlanes Allocated rows: 1  Used rows: 1  Row size: 1392 bytes
  //  Table: tpcPadPlanes_st[0]--> tpcPadPlanes_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcPadPlanes")) return 0;
  tpcPadPlanes_st row;
  St_tpcPadPlanes *tableSet = new St_tpcPadPlanes("tpcPadPlanes",1);
  enum {NinnerRows = 50};
  Int_t nPadsInner[NinnerRows]  = { 80, 82, 84, 86, 88, 90, 92, 94, 96, 98,
				    100,102,104,106,108,110,112,114,116,118,
				    120,122,124,126,128,130,132,134,138,140,
				    142,144,146,148,150,152,154,156,158,160,
				    162,164,166,168,170,172,174,176,178,180};
  Int_t nPadsOuter[32]  = {    98 ,    100,    102,    104,    106,    106,    108,    110,    112,    112,
			       114,    116,    118,    120,    122,    122,    124,    126,    128,    128,
			       130,    132,    134,    136,    138,    138,    140,    142,    144,    144,    
			       144,    144};
  Double_t RImin =  56.200;
  Double_t ROmin = 127.195;
  memset(&row,0,tableSet->GetRowSize());
  row.innerPadRows	     = NinnerRows; // ;		       
  //  row.innerPadRows48	     =          8; // ;		       
  //  row.innerPadRows52	     =          5; // ;		       
  row.superInnerPadRows	     =          3; // ;		       
  row.innerSectorPadPitch    =       0.335;
  row.innerSectorPadLength   =       1.23; //  1.15;	       
  row.innerSectorPadWidth    = row.innerSectorPadPitch - 0.05;
  row.innerSectorRowPitch1   = row.innerSectorPadLength + 0.05;  
  //  row.innerSectorRowPitch2   = row.innerSectorPadLength + 0.05;  
  row.innerSectorEdge	     =     51.905; // ;		       
  row.innerSectorPadPlaneZ   =    209.707; // ;                  
  row.firstPadRow	     = RImin + row.innerSectorRowPitch1/2;
  for (Int_t i = 0; i < row.innerPadRows; i++) {
    row.innerPadsPerRow[i]   = nPadsInner[i];
    row.innerRowRadii[i]     = row.firstPadRow + row.innerSectorRowPitch1*i;
  }
  row.firstRowWidth	     =   row.innerPadsPerRow[0]*row.innerSectorPadPitch;
  row.outerPadRows	     =      32; // ;
  row.padRows	             = row.innerPadRows + row.outerPadRows;
  row.outerSectorPadWidth    =    0.62; // ;
  row.outerSectorPadLength   =    1.95; // ;
  row.outerSectorPadPitch    = row.outerSectorPadWidth + 0.05;     
  row.outerSectorLength	     =      62; // ;
  row.superOuterPadRows	     =       1; // ;
  row.firstOuterSectorPadRow = ROmin;
  row.outerSectorRowPitch    = row.outerSectorPadLength + 0.05;    // ;
  row.outerSectorEdge	     = 121.732; // ;
  row.outerSectorPadPlaneZ   = 210.107; // ;
  for (Int_t i = 0; i < row.outerPadRows; i++) {
    row.outerPadsPerRow[i] = nPadsOuter[i];
    row.outerRowRadii[i]   = row.firstOuterSectorPadRow + row.outerSectorRowPitch*i;
  }
  row.lastRowWidth	   = row.outerPadsPerRow[row.outerPadRows-1]*row.outerSectorPadPitch;
  row.ioSectorSeparation   = 
    (row.outerRowRadii[                 0] - row.outerSectorRowPitch/2) -
    (row.innerRowRadii[row.innerPadRows-1] + row.innerSectorRowPitch1/2);
  row.lastOuterSectorPadRow	 =  row.outerRowRadii[row.outerPadRows-1];   
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
