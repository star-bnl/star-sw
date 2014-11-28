TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Geometry/tpc/.tpcPadPlanes/tpcPadPlanes Allocated rows: 1  Used rows: 1  Row size: 1392 bytes
  //  Table: tpcPadPlanes_st[0]--> tpcPadPlanes_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcPadPlanes")) return 0;
  tpcPadPlanes_st row;
  St_tpcPadPlanes *tableSet = new St_tpcPadPlanes("tpcPadPlanes",1);
  /* 
     Tonko 09/05/14 

Pads are spaced 0.5 x 1.6 cm with a 0.5 mm gap.
Which means the actual copper size of the pad is 0.45 x 1.55 cm.

Row 1: 56 pads
Row 2: 58 pads
Row 3: 60 pads
Row 4: 62 pads
Row 5: 62 pads
Row 6: 64 pads
Row 7: 66 pads
Row 8: 68 pads
Row 9: 70 pads
Row 10: 72 pads
Row 11: 74 pads
Row 12: 74 pads
Row 13: 76 pads
Row 14: 78 pads
Row 15: 80 pads
Row 16: 82 pads
Row 17: 84 pads
Row 18: 86 pads
Row 19: 86 pads
Row 20: 88 pads
Row 21: 90 pads
Row 22: 92 pads
Row 23: 94 pads
Row 24: 96 pads
Row 25: 98 pads
Row 26: 98 pads
Row 27: 100 pads
Row 28: 102 pads
Row 29: 104 pads
Row 30: 106 pads
Row 31: 108 pads
Row 32: 110 pads
Row 33: 112 pads
Row 34: 112 pads
Row 35: 114 pads
Row 36: 116 pads
Row 37: 118 pads
Row 38: 120 pads
Row 39: 122 pads
Row 40: 122 pads
grep Row tpcPadPlanes.devTX.C | grep pads | awk 'BEGIN {n=0;}{n += $3} END {print "Total =" n}' 

Total =3580
*/
  enum {NinnerRows = 40};
  Int_t nPadsInner[NinnerRows]  = { 56, 58, 60, 62, 62, 64, 66, 68, 70, 72,
				    74, 74, 76, 78, 80, 82, 84, 86, 86, 88,
				    90, 92, 94, 96, 98, 98,100,102,104,106,
				   108,110,112,112,114,116,118,120,122,122};

  Int_t nPadsOuter[32]  = {    98 ,    100,    102,    104,    106,    106,    108,    110,    112,    112,
			       114,    116,    118,    120,    122,    122,    124,    126,    128,    128,
			       130,    132,    134,    136,    138,    138,    140,    142,    144,    144,    
			       144,    144};
  memset(&row,0,tableSet->GetRowSize());
  row.innerPadRows	     = NinnerRows; // 		       
  row.superInnerPadRows	     =          3; // 		       
  row.innerSectorPadPitch    =       0.50;
  row.innerSectorPadLength   =       1.55; //         
  row.innerSectorPadWidth    = row.innerSectorPadPitch  - 0.05;
  row.innerSectorRowPitch1   = row.innerSectorPadLength + 0.05;  
  row.innerSectorEdge	     =     51.905; // ;		       
  row.innerSectorPadPlaneZ   =    209.707; // ;                  
  Double_t RImax = 119.6 + 1.15/2; // maximum radius for old inner TPC
  Double_t RImin = RImax - NinnerRows*row.innerSectorRowPitch1;
  Double_t ROmin = 127.195;
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
