#ifdef EEmapFEE_USE

// used to emulate Endcap FEE TP math
struct EEfeeTPmap {
  int JP;
  const char* name;
  int lenCh, cha0L, cha0H;
};

// this is mapping of 90 Endcap TP to ADC channels, utilized by constructor of EEfeeTP class.

struct  EEfeeTPmap eeTPmap[90]={
  /* syntax: EEfeeTP(crate, TPname, lenCh, chL, [chH])
    feeTP[nn] uses Hank's indexing
    see Steve's  color map of  TP for explanation  */

  /*:::::::::: J E T.....P A T C H......T H R E E .........*/  
  /*......... DSM0 board 1 .... */
  {3,"JP3_TP1D1/0",10,3,11},
  {3,"JP3_TP2D1/1",8,80,-1},
  {3,"JP3_TP4D1/2",10,19,27},
  {3,"JP3_TP5D1/3",8,88,-1},
  {3,"JP3_TP7D1/4",10,35,43},
  {3,"JP3_TP8D1/5",8,96,-1},
  {3,"JP3_TP10D1/6",10,51,59},
  {3,"JP3_TP11D1/7",8,104,-1},
  {3,"JP3_TP13D1/8",10,67,75},
  {3,"JP3_TP14D1/9",8,112,-1},
  
  /*......... DSM0 board 2    */
  {3,"JP3_TP3D2/0",6,0,8},
  {3,"JP3_TP6D2/1",6,16,24},
  {3,"JP3_TP9D2/2",6,32,40},
  {3,"JP3_TP12D2/3",6,48,56},
  {3,"JP3_TP15D2/4",6,64,72},

  /*:::::::::: J E T.....P A T C H......F O U R .........*/
  
  {4,"JP4_TP3D2/5",6,0,8},
  {4,"JP4_TP6D2/6",6,16,24},
  {4,"JP4_TP9D2/7",6,32,40},
  {4,"JP4_TP12D2/8",6,48,56},
  {4,"JP4_TP15D2/9",6,64,72},
  
  /*......... DSM0 board 3 .....  */
  {4,"JP4_TP1D3/0",10,3,11},
  {4,"JP4_TP2D3/1",8,80,-1},
  {4,"JP4_TP4D3/2",10,19,27},
  {4,"JP4_TP5D3/3",8,88,-1},
  {4,"JP4_TP7D3/4",10,35,43},
  {4,"JP4_TP8D3/5",8,96,-1},
  {4,"JP4_TP10D3/6",10,51,59},
  {4,"JP4_TP11D3/7",8,104,-1},
  {4,"JP4_TP13D3/8",10,67,75},
  {4,"JP4_TP14D3/9",8,112,-1},    
 
  /*:::::::::: J E T.....P A T C H......F I V E .........*/  
  /*......... DSM0 board 4 .... */
  {5,"JP5_TP1D4/0",10,3,11},
  {5,"JP5_TP2D4/1",8,80,-1},
  {5,"JP5_TP4D4/2",10,19,27},
  {5,"JP5_TP5D4/3",8,88,-1},
  {5,"JP5_TP7D4/4",10,35,43},
  {5,"JP5_TP8D4/5",8,96,-1},
  {5,"JP5_TP10D4/6",10,51,59},
  {5,"JP5_TP11D4/7",8,104,-1},
  {5,"JP5_TP13D4/8",10,67,75},
  {5,"JP5_TP14D4/9",8,112,-1},
  
  /*......... DSM0 board 5    */
  {5,"JP5_TP3D5/0",6,0,8},
  {5,"JP5_TP6D5/1",6,16,24},
  {5,"JP5_TP9D5/2",6,32,40},
  {5,"JP5_TP12D5/3",6,48,56},
  {5,"JP5_TP15D5/4",6,64,72},

  /*:::::::::: J E T.....P A T C H......S I X .........*/
  
  {6,"JP6_TP3D5/5",6,0,8},
  {6,"JP6_TP6D5/6",6,16,24},
  {6,"JP6_TP9D5/7",6,32,40},
  {6,"JP6_TP12D5/8",6,48,56},
  {6,"JP6_TP15D5/9",6,64,72},
  
  /*......... DSM0 board 6 .....  */
  {6,"JP6_TP1D6/0",10,3,11},
  {6,"JP6_TP2D6/1",8,80,-1},
  {6,"JP6_TP4D6/2",10,19,27},
  {6,"JP6_TP5D6/3",8,88,-1},
  {6,"JP6_TP7D6/4",10,35,43},
  {6,"JP6_TP8D6/5",8,96,-1},
  {6,"JP6_TP10D6/6",10,51,59},
  {6,"JP6_TP11D6/7",8,104,-1},
  {6,"JP6_TP13D6/8",10,67,75},
  {6,"JP6_TP14D6/9",8,112,-1},    

  /*:::::::::: J E T.....P A T C H......O N E .........*/  
  /*......... DSM0 board 7 .... */
  {1,"JP1_TP1D7/0",10,3,11},
  {1,"JP1_TP2D7/1",8,80,-1},
  {1,"JP1_TP4D7/2",10,19,27},
  {1,"JP1_TP5D7/3",8,88,-1},
  {1,"JP1_TP7D7/4",10,35,43},
  {1,"JP1_TP8D7/5",8,96,-1},
  {1,"JP1_TP10D7/6",10,51,59},
  {1,"JP1_TP11D7/7",8,104,-1},
  {1,"JP1_TP13D7/8",10,67,75},
  {1,"JP1_TP14D7/9",8,112,-1},
  
  /*......... DSM0 board 8    */
  {1,"JP1_TP3D8/0",6,0,8},
  {1,"JP1_TP6D8/1",6,16,24},
  {1,"JP1_TP9D8/2",6,32,40},
  {1,"JP1_TP12D8/3",6,48,56},
  {1,"JP1_TP15D8/4",6,64,72},
  
  /*:::::::::: J E T.....P A T C H......T W O .........*/
  
  {2,"JP2_TP3D8/5",6,0,8},
  {2,"JP2_TP6D8/6",6,16,24},
  {2,"JP2_TP9D8/7",6,32,40},
  {2,"JP2_TP12D8/8",6,48,56},
  {2,"JP2_TP15D8/9",6,64,72},
  
  /*......... DSM0 board 9 .....  */
  {2,"JP2_TP1D9/0",10,3,11},
  {2,"JP2_TP2D9/1",8,80,-1},
  {2,"JP2_TP4D9/2",10,19,27},
  {2,"JP2_TP5D9/3",8,88,-1},
  {2,"JP2_TP7D9/4",10,35,43},
  {2,"JP2_TP8D9/5",8,96,-1},
  {2,"JP2_TP10D9/6",10,51,59},
  {2,"JP2_TP11D9/7",8,104,-1},
  {2,"JP2_TP13D9/8",10,67,75},
  {2,"JP2_TP14D9/9",8,112,-1},    

};

#endif

#ifdef EEmapTP_USE


//===========================================================

// hardocded map of TP for EEMC DSM-0

#define  EEnTPeta 3  // numbers TP patches in eta  (from center)
#define  EEnTPphi 30  // numbers TP patches in phi (clock wise)

struct  EEmapTP { 
  int JPid, TPid; // Jet Patch ID [1-6] & Trig Patch ID [1-15] according to Steve's numbering scheme 
  int brdIn,chIn; //DSM level-0 input board [1-9] & channel number 0-9]
  int chOut; // DSM level-0 output channel [0-11] 
}; 
//

static EEmapTP eeMapTP[EEnTPphi][EEnTPeta] ={
 { {1, 1, 7, 0, 8}, {1, 2, 7, 1, 8}, {1, 3, 8, 0, 9} }, 
 { {1, 4, 7, 2, 8}, {1, 5, 7, 3, 8}, {1, 6, 8, 1, 9} }, 
 { {1, 7, 7, 4, 8}, {1, 8, 7, 5, 8}, {1, 9, 8, 2, 9} }, 
 { {1,10, 7, 6, 8}, {1,11, 7, 7, 8}, {1,12, 8, 3, 9} }, 
 { {1,13, 7, 8, 8}, {1,14, 7, 9, 8}, {1,15, 8, 4, 9} }, 
 { {2, 1, 9, 0,11}, {2, 2, 9, 1,11}, {2, 3, 8, 5,10} }, 
 { {2, 4, 9, 2,11}, {2, 5, 9, 3,11}, {2, 6, 8, 6,10} }, 
 { {2, 7, 9, 4,11}, {2, 8, 9, 5,11}, {2, 9, 8, 7,10} }, 
 { {2,10, 9, 6,11}, {2,11, 9, 7,11}, {2,12, 8, 8,10} }, 
 { {2,13, 9, 8,11}, {2,14, 9, 9,11}, {2,15, 8, 9,10} }, 
 { {3, 1, 1, 0, 0}, {3, 2, 1, 1, 0}, {3, 3, 2, 0, 1} }, 
 { {3, 4, 1, 2, 0}, {3, 5, 1, 3, 0}, {3, 6, 2, 1, 1} }, 
 { {3, 7, 1, 4, 0}, {3, 8, 1, 5, 0}, {3, 9, 2, 2, 1} }, 
 { {3,10, 1, 6, 0}, {3,11, 1, 7, 0}, {3,12, 2, 3, 1} }, 
 { {3,13, 1, 8, 0}, {3,14, 1, 9, 0}, {3,15, 2, 4, 1} }, 
 { {4, 1, 3, 0, 3}, {4, 2, 3, 1, 3}, {4, 3, 2, 5, 2} }, 
 { {4, 4, 3, 2, 3}, {4, 5, 3, 3, 3}, {4, 6, 2, 6, 2} }, 
 { {4, 7, 3, 4, 3}, {4, 8, 3, 5, 3}, {4, 9, 2, 7, 2} }, 
 { {4,10, 3, 6, 3}, {4,11, 3, 7, 3}, {4,12, 2, 8, 2} }, 
 { {4,13, 3, 8, 3}, {4,14, 3, 9, 3}, {4,15, 2, 9, 2} }, 
 { {5, 1, 4, 0, 4}, {5, 2, 4, 1, 4}, {5, 3, 5, 0, 5} }, 
 { {5, 4, 4, 2, 4}, {5, 5, 4, 3, 4}, {5, 6, 5, 1, 5} }, 
 { {5, 7, 4, 4, 4}, {5, 8, 4, 5, 4}, {5, 9, 5, 2, 5} }, 
 { {5,10, 4, 6, 4}, {5,11, 4, 7, 4}, {5,12, 5, 3, 5} }, 
 { {5,13, 4, 8, 4}, {5,14, 4, 9, 4}, {5,15, 5, 4, 5} }, 
 { {6, 1, 6, 0, 7}, {6, 2, 6, 1, 7}, {6, 3, 5, 5, 6} }, 
 { {6, 4, 6, 2, 7}, {6, 5, 6, 3, 7}, {6, 6, 5, 6, 6} }, 
 { {6, 7, 6, 4, 7}, {6, 8, 6, 5, 7}, {6, 9, 5, 7, 6} }, 
 { {6,10, 6, 6, 7}, {6,11, 6, 7, 7}, {6,12, 5, 8, 6} }, 
 { {6,13, 6, 8, 7}, {6,14, 6, 9, 7}, {6,15, 5, 9, 6} }
};

#endif

/*

The .h file tells you how to find  relation between 
Steve's TP id and DSM input.

brdIn,chIn - refer to level 0 inputs
chOut - is level 1 input (i.e. level 0 output)

The tricky part is to find relation between tower ID and
one of elements in this array.
An 'lement'is this 5 numbers:   {1, 2, 7, 1, 8}

As you see  eeMapTP[EEnTPphi=i][EEnTPeta=j]  forms a 2D array describing topology of 90 EEMC TP .

EEnTPeta  numbers TP from inside, 
tower eta bins 1-3 -->j=0, 4-7 -->j=1, 8-12 -->j=2;

EEnTPphi numbers 30 TP in phi . 60/2=30 - makes sense.
I forgot where is the zero, I think it should be for the first TP in JP1.
then you go clokwise.

The firts element eeMapTP[0][0]= {1, 1, 7, 0, 8} corresponds to towers:
11TD01 11TE01
11TD02 11TE02
11TD03 11TE03

(Assuming JP=1 covers 11TD -1TC  - I do not remeber)

Next element in eta direction eeMapTP[1][0]= {1, 2, 7, 1, 8} covers
11TD04 11TE04
11TD05 11TE05
11TD06 11TE06
11TD07 11TE07

Next elemnt in phi direction , clockwise eeMapTP[0][1]= {1, 4, 7, 2, 8}
12TA01 12TB01
12TA02 12TB02
12TA03 12TB03

etc.
*/
