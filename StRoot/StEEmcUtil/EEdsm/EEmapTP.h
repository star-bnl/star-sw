
#ifdef EEmapTP_USE
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
