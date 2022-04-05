//////////////////////////////////////////////////////////////////////
//
// $Id: StPeCL0.cxx,v 1.6 2003/09/02 17:58:46 perev Exp $
// $Log: StPeCL0.cxx,v $
// Revision 1.6  2003/09/02 17:58:46  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.5  2003/04/30 20:37:54  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.4  2002/12/16 23:04:01  yepes
// Field comes in KGauss and should be passed to routines in Teslas
// problem pointed out by Vladimir
//
// Revision 1.3  2001/04/25 18:11:25  perev
// HPcorrs
//
// Revision 1.2  2001/02/21 20:54:24  yepes
// *** empty log message ***
//
// Revision 1.0  2000/12/11 
//
//////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <math.h>
#include "StPeCL0.h"
#include "StCtbTriggerDetector.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"



ClassImp(StPeCL0)

StPeCL0::StPeCL0() {
   infoLevel = 0 ;
   minAdc    = 3 ;
   maxAdc    = 16 ;
}

StPeCL0::~StPeCL0() {

}

Int_t StPeCL0::dsm1Sum ( ) {
//
//  Run sums at first dsm level
   int i, j;
   for(i=0; i<nL0Phi; i++){for(j=0; j<nL0Eta; j++){array2[i][j] = 0;}}
   for(i=0;   i<15;  i++){for(j=0; j<nL0Slats; j++){array2[0][2+j]+=weighted1[i][j];}}
   for(i=15;  i<30;  i++){for(j=0; j<nL0Slats; j++){array2[3][2+j]+=weighted1[i][j];}}
   for(i=30;  i<45;  i++){for(j=0; j<nL0Slats; j++){array2[2][2+j]+=weighted1[i][j];}}
   for(i=45;  i<60;  i++){for(j=0; j<nL0Slats; j++){array2[1][2+j]+=weighted1[i][j];}}
   for(i=60;  i<75;  i++){for(j=0; j<nL0Slats; j++){array2[1][1-j]+=weighted1[i][j];}}
   for(i=75;  i<90;  i++){for(j=0; j<nL0Slats; j++){array2[2][1-j]+=weighted1[i][j];}}
   for(i=90;  i<105; i++){for(j=0; j<nL0Slats; j++){array2[3][1-j]+=weighted1[i][j];}}
   for(i=105; i<120; i++){for(j=0; j<nL0Slats; j++){array2[0][1-j]+=weighted1[i][j];}}
//
//   Look at lookup tables
//
   int sum ;
   for ( i = 0 ; i < nL0Phi ; i++ ) {
      for ( j = 0 ; j < nL0Eta ; j++ ) {
         sum = array2[i][j] ;
	 if      ( sum > 255 ) weighted2[i][j] = 255 ;
	 else if ( sum < 0   ) weighted2[i][j] =   0 ;
	 else                  weighted2[i][j] = lut2[i][j][sum] ;
      }
   }
   return 0 ;
}

Int_t StPeCL0::dsm2Sum ( ){
  array3[0]=0; array3[1]=0;
  {for(int i=0; i<nL0Phi; i++){for(int j=0; j<2; j++){array3[0]+=weighted2[i][j];}}}
  {for(int i=0; i<nL0Phi; i++){for(int j=2; j<4; j++){array3[1]+=weighted2[i][j];}}}
//
//   Look at lookup tables
//
   int sum ;
   {for ( int i = 0 ; i < 2 ; i++ ) {
      sum = array3[i] ;
      if      ( sum > 255 ) weighted3[i] = 255 ;
      else if ( sum < 0   ) weighted3[i] =   0 ;
      else                  weighted3[i] = lut3[i][sum] ;
   }}
   return 0 ;
}


void StPeCL0::printWeightedSlats ( ){
  int i, j;

  char parray[nL0Trays][nL0Slats] ;
  for ( i = 0 ; i < nL0Trays ; i++ ) {
     for ( j = 0 ; j < nL0Slats ; j++ ) {
        parray[i][j] = weighted1[i][j] ;
	if ( parray[i][j] > 14 ) parray[i][j] = 14 ;
     }
  }

  cout << " ++++++++++ Weighted Slat Information ++++++++++ " <<endl ;
  cout << "              :       south ->  top -> north -> bottom" << endl;  
  cout << "              :    13-1 60-59   |       58-44     |       43-29     |       28-14" << endl;
  for(j=1; j>-1; j--){
    cout << "west / eta= " << j << " : ";
    for(i=12; i>-1; i--) printf ( "%x", parray[i][j] ); 
    for(i=59; i>57; i--) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=57; i>42; i--) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=42; i>27; i--) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=27; i>12; i--) printf ( "%x", parray[i][j] ); 
    printf ( "\n" ) ;
  } 
  for(j=0; j<2; j++){
    cout << "east / eta= " << j << " : ";
    for(i=102; i<117; i++) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=117; i<120; i++) printf ( "%x", parray[i][j] ); 
    for(i=60;  i< 72; i++) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=72;  i< 87; i++) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=87;  i<102; i++) printf ( "%x", parray[i][j] ); 
    cout << endl;
  } 
  cout << "              :      103-117    |  118-120 61-72  |      73-87      |      88-102" << endl;
}

void StPeCL0::printSlats ( ){
  int i, j;

  char parray[nL0Trays][nL0Slats] ;
  for ( i = 0 ; i < nL0Trays ; i++ ) {
     for ( j = 0 ; j < nL0Slats ; j++ ) {
        parray[i][j] = array1[i][j] ;
	if ( parray[i][j] > 14 ) parray[i][j] = 14 ;
     }
  }
  cout << " ++++++++++ Slat Information ++++++++++ " <<endl ;
  cout << "              :       south  ->   top  ->   north  ->  bottom" << endl;  
  cout << "              :    13-1 60-59   |       58-44     |       43-29     |       28-14" << endl;
  for(j=1; j>-1; j--){
    cout << "west / eta= " << j << " : ";
    for(i=12; i>-1; i--) printf ( "%x", parray[i][j] ); 
    for(i=59; i>57; i--) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=57; i>42; i--) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=42; i>27; i--) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=27; i>12; i--) printf ( "%x", parray[i][j] ); 
    printf ( "\n" ) ;
  } 
  for(j=0; j<2; j++){
    cout << "east / eta= " << j << " : ";
    for(i=102; i<117; i++) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=117; i<120; i++) printf ( "%x", parray[i][j] ); 
    for(i=60;  i< 72; i++) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=72;  i< 87; i++) printf ( "%x", parray[i][j] ); 
    printf ( " | " ) ;
    for(i=87;  i<102; i++) printf ( "%x", parray[i][j] ); 
    cout << endl;
  } 
  cout << "              :      103-117    |  118-120 61-72  |      73-87      |      88-102" << endl;
}

void StPeCL0::printPatches ( ) {  
  cout << "  ------- Patches --------- " << endl;
  cout << "       :  south top/north bottom/south bottom" << endl;
  for(int j=0; j<nL0Eta; j++){    
    cout << "eta= " << j << " : ";
    for(int i=0; i<nL0Phi; i++){
      cout << " " << (int)array2[i][j]<<" "<<i<<j<<" "<<(int)(lut2[i][j][array2[i][j]]);
    }
    cout << endl;
  }
}

void StPeCL0::printWeightedPatches ( ) {  
  cout << "  ------- Weighted Patches --------- " << endl;
  cout << "       :  south top/north bottom/south bottom" << endl;
  for(int j=0; j<nL0Eta; j++){    
    cout << "eta= " << j << " : ";
    for(int i=0; i<nL0Phi; i++){
      cout << " " << (int)weighted2[i][j];
    }
    cout << endl;
  }
}
 

#ifndef __CINT__
Int_t StPeCL0::process ( StEvent* event ) {

   StTriggerDetectorCollection* trg = event->triggerDetectorCollection();
   StCtbTriggerDetector& ctb = trg->ctb();

   if ( !&ctb ){
      printf ( "StPeCL0::process: Didn't find CTB" ) ;
      return  1;

   }
   int i, j, slot ;
   for ( i=0 ; i<nL0Trays; i++ ){
      slot = cabling[i] ;
      if ( slot < 0 || slot >= nL0Trays ) {
         printf ( "StPeCL0:process: wrong cabling tray %d slot %d \n", i, slot ) ;
	 continue ;
      }
      for ( j = 0 ; j < nL0Slats ; j++ ){
	 int adc = (int)ctb.mips(i,j,0) ;
	 if ( adc < 0  ) {
//   printf ( "StPeCL0:process: adc %d out of range \n", adc ) ;
            adc = 0 ;
	 }
	 if ( adc > 255  ) {
	    printf ( "StPeCL0:process: adc %d out of range \n", adc ) ;
            adc = 255 ;
	 }
	 array1[i][j] = adc ; 
	 weighted1[slot][j] = lut1[slot][j][adc] ;
    //   if ( infoLevel && adc > 0 && weighted1[slot][j] > 0 ) 
         if ( infoLevel && adc > 0  ) 
	    printf ( " Event %d tray %d slot %d slat %d adc %d matched %d \n", 
	               event->id(), i+1, slot, j+1, adc, weighted1[slot][j] ) ;
      }
   }


   if ( infoLevel ) {
      printSlats();
      printWeightedSlats();
   }

   dsm1Sum() ;

   if ( infoLevel ) {
      printPatches() ;
      printWeightedPatches();
   }

   dsm2Sum() ;

   int output = weighted3[0] + weighted3[1] ;
   if ( infoLevel ) 
      printf ( "StPeCL0::process: trigger output %d \n", output ) ;

   return output ;
}

Int_t StPeCL0::process(StMuDst* mudst)
{
	StMuEvent* event = 0;
	event = mudst->event();


	StCtbTriggerDetector& ctb = event->ctbTriggerDetector();
	if (!&ctb)
	{
		printf("StPeCL0::process: Didn't find CTB");
		return 1;
	}


	int i, j, slot;

	for (i=0 ; i<nL0Trays; i++)
	{
		slot = cabling[i];
		if (slot < 0 || slot >= nL0Trays)
		{
			printf ("StPeCL0:process: wrong cabling tray %d slot %d \n", i, slot ) ;
			continue;
		}

		for (j = 0; j < nL0Slats; j++)
		{
			int adc = (int)ctb.mips(i, j, 0);
			if (adc < 0)
				adc = 0;

			if (adc > 255)
			{
				printf ("StPeCL0:process: adc %d out of range \n", adc);
				adc = 255;
			}

			array1[i][j] = adc;
			weighted1[slot][j] = lut1[slot][j][adc];
			if (infoLevel && adc > 0) 
				printf (" Event %d tray %d slot %d slat %d adc %d matched %d \n", mudst->event()->eventInfo().id(), i+1, slot, j+1, adc, weighted1[slot][j]);
		}
	}


	if (infoLevel) 
	{
		printSlats();
		printWeightedSlats();
	}

	dsm1Sum();

	if (infoLevel)
	{
		printPatches();
		printWeightedPatches();
	}

	dsm2Sum();

	int output = weighted3[0] + weighted3[1];
	if (infoLevel) 
		printf ("StPeCL0::process: trigger output %d \n", output);


	return output;
}
#endif

void StPeCL0::setLinearLuts() {

   int i, j, k ;
   for ( i = 0 ; i < nL0Trays ; i++ ) {
      for ( j = 0 ; j < nL0Range ; j++ ) {
         lut1[i][0][j] = j ;
         lut1[i][1][j] = j ;
      }
   }
   for ( i = 0 ; i < nL0Phi ; i++ ) {
      for ( j = 0 ; j < nL0Eta ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) {
            lut2[i][j][k] = k ;
            lut2[i][j][k] = k ;
         }
      }
   }

   for ( k = 0 ; k < nL0Range ; k++ ) {
      lut3[0][k] = k ;
      lut3[1][k] = k ;
   }
}

void StPeCL0::setP4Luts() {

   int i, j, k ;
//
//
// 1. map "top" and "bottom" slats to >16 for 3<adc<16 to effectively veto
//  any events having hits on the top or bottom. The matching from east to
//  west top and bottom is not exact, but I took the easiest path here just
//  to get us going. 
//      top West: 50-60,1-8             bot West: 20-34
//      top east: 110-120,61-68         bot East: 80-94
//
//   Top West
//
   for ( i = 0 ; i < 8 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }

   for ( i = 49 ; i < 60 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Bottom West
//
   for ( i = 19 ; i < 34 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Top East   
//
   for ( i = 109 ; i < 120 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
   for ( i =  60 ; i <  68 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Bottom East   
//
   for ( i =  79 ; i <  94 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//  2.map 3<adc<9 -> 1 for "north" side slats
//  map 3<adc<9 -> 4 for "South" side slats
//
//    NorthWest 
//
   for ( i =  34 ; i <  50 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 1 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//    SouthWest 
//
   for ( i =   8 ; i <  19 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 4 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//    NorthEast 
//
   for ( i =  68 ; i <  79 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 1 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//    SouthEast 
//
   for ( i =  94 ; i < 109 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 4 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//   Depth 2 LUT
//
   for ( i = 0 ; i < nL0Phi ; i++ ) {
      for ( j = 0 ; j < nL0Eta ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k == 1 ) lut2[i][j][k] = 1 ;
            else if ( k == 4 ) lut2[i][j][k] = 4 ;
            else if ( k == 5 ) lut2[i][j][k] = 5 ;
            else if ( k == 8 ) lut2[i][j][k] = 8 ;
	    else lut2[i][j][k] = 0 ; 
	 }
      }
   }
//
//   Depth 3 LUT
//
   for ( i = 0 ; i < 2 ; i++ ) {
      for ( k = 0 ; k < nL0Range ; k++ ) { 
         lut3[i][k] = k ;
      }
   }
}
void StPeCL0::setCountingLuts() {

   int i, j, k ;
   for ( i = 0 ; i < nL0Trays ; i++ ) {
      for ( j = 0 ; j < nL0Range ; j++ ) {
         if ( j < minAdc ) { 
            lut1[i][0][j] = 0 ;
            lut1[i][1][j] = 0 ;
	 }
	 else {
            lut1[i][0][j] = 1 ;
            lut1[i][1][j] = 1 ;
	 }
      }
   }
   for ( i = 0 ; i < nL0Phi ; i++ ) {
      for ( j = 0 ; j < nL0Eta ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) {
	    if ( i == 0 || i == 2 ) { // bottom or top
	       if ( k > 0 ) lut2[i][j][k] = 20 ;
            }
	    else if ( i == 1 ) {
	       if ( k < 10 ) lut2[i][j][k] = 10 * k ;
	       else          lut2[i][j][k] = 255 ;
	    }   
	    else lut2[i][j][k] = k ;
         }
      }
   }

   for ( k = 0 ; k < nL0Range ; k++ ) {
      lut3[0][k] = k ;
      lut3[1][k] = k ;
   }
}

void StPeCL0::setP4PLuts() {

   int i, j, k ;
//
//
// 1. map "top" and "bottom" slats to >16 for 3<adc<16 to effectively veto
//  any events having hits on the top or bottom. The matching from east to
//  west top and bottom is not exact, but I took the easiest path here just
//  to get us going. 
//      top West: 50-60,1-8             bot West: 20-34
//      top east: 110-120,61-68         bot East: 80-94
//
//   Top West
//
   for ( i = 0 ; i < 3 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }

   for ( i = 52 ; i < 60 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Bottom West
//
   for ( i = 22 ; i < 33 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Top East   
//
   for ( i = 112 ; i < 120 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
   for ( i =  60 ; i <  63 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Bottom East   
//
   for ( i =  82 ; i <  93 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//  2.map 3<adc<9 -> 1 for "north" side slats
//  map 3<adc<9 -> 4 for "South" side slats
//
//    NorthWest 
//
   for ( i =  33 ; i <  52 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 4 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//    SouthWest 
//
   for ( i =   3 ; i <  22 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 1 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//    NorthEast 
//
   for ( i =  63 ; i <  82 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 1 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//    SouthEast 
//
   for ( i =  93 ; i < 112 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k < minAdc  ) lut1[i][j][k] = 0 ;
            else if ( k < maxAdc  ) lut1[i][j][k] = 4 ;
            else               lut1[i][j][k] = 17 ;
         }
      }
   }
//
//   Depth 2 LUT
//
   for ( i = 0 ; i < nL0Phi ; i++ ) {
      for ( j = 0 ; j < nL0Eta ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if      ( k == 1 ) lut2[i][j][k] = 1 ;
            else if ( k == 4 ) lut2[i][j][k] = 4 ;
            else if ( k == 5 ) lut2[i][j][k] = 5 ;
            else if ( k == 8 ) lut2[i][j][k] = 8 ;
	    else lut2[i][j][k] = 0 ; 
	 }
      }
   }
//
//   Depth 3 LUT
//
   for ( i = 0 ; i < 2 ; i++ ) {
      for ( k = 0 ; k < nL0Range ; k++ ) { 
         lut3[i][k] = k ;
      }
   }
}
//
//    P4 luts with switched weigths
//       Configuration actually used in 2000
//
void StPeCL0::setP4SLuts() {

   int i, j, k ;
//
//
// 1. map "top" and "bottom" slats to >16 for 3<adc<16 to effectively veto
//  any events having hits on the top or bottom. The matching from east to
//  west top and bottom is not exact, but I took the easiest path here just
//  to get us going. 
//      top West: 50-60,1-8             bot West: 20-34
//      top east: 110-120,61-68         bot East: 80-94
//
//   Top West
//
   for ( i = 0 ; i < 3 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }

   for ( i = 52 ; i < 60 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Bottom West
//
   for ( i = 22 ; i < 33 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Top East   
//
   for ( i = 112 ; i < 120 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
   for ( i =  60 ; i <  63 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//   Bottom East   
//
   for ( i =  82 ; i <  93 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) { 
         for ( k = 0 ; k < nL0Range ; k++ ) {
            if ( k < minAdc ) lut1[i][j][k] = 0 ;
            else         lut1[i][j][k] = maxAdc ;
         }
      }
   }
//
//  2.map 3<adc<9 -> 1 for "north" side slats
//  map 3<adc<9 -> 4 for "South" side slats
//
//    NorthWest 
//
   for ( i =  33 ; i <  52 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            lut1[i][j][k] = k ;
         }
      }
   }
//
//    SouthWest 
//
   for ( i =   3 ; i <  22 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            lut1[i][j][k] = k ;
         }
      }
   }
//
//    NorthEast 
//
   for ( i =  63 ; i <  82 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            lut1[i][j][k] = k ;
         }
      }
   }
//
//    SouthEast 
//
   for ( i =  93 ; i < 112 ; i++ ) {
      for ( j = 0 ; j < nL0Slats ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            lut1[i][j][k] = k ;
         }
      }
   }
//
//   Depth 2 LUT
//
   int ns[4][4] = {{4,4,1,1},{1,1,4,4},{1,1,4,4},{4,4,1,1}};
   int adc_sum_min=2, adc_sum_max=9;

   for ( i = 0 ; i < nL0Phi ; i++ ) {
      for ( j = 0 ; j < nL0Eta ; j++ ) {
         for ( k = 0 ; k < nL0Range ; k++ ) { 
            if ( k <= adc_sum_min  ) lut2[i][j][k] = 0 ; 
            else if ( k <= adc_sum_max ) lut2[i][j][k] = ns[i][j] ; 
            else if ( k <= 16 ) lut2[i][j][k] = 0 ; 
            else lut2[i][j][k] = k ; 
	 }
      }
   }
//
//   Depth 3 LUT
//
   for ( i = 0 ; i < 2 ; i++ ) {
      for ( k = 0 ; k < nL0Range ; k++ ) { 
         if ( k == 1 || k == 4 || k == 5 ) lut3[i][k] = k ;
	 else lut3[i][k] = 0 ;
      }
   }
}
//
void StPeCL0::setYear1Input() {
   for ( int i = 0 ; i < nL0Trays ; i++ ) {
      cabling[i] = i ;
   }
}
//
void StPeCL0::setYear2Input() {
//
// West side
//
   {for ( int i = 0 ; i < 50 ; i++ ) {
      cabling[i] = i + 10 ;
   }}
   {for ( int i = 50 ; i < 60 ; i++ ) {
      cabling[i] = i - 50 ;
   }}
//
// East side
//
   {for ( int i = 65 ; i < 120 ; i++ ) {
      cabling[i] = i - 5 ;
   }}
   {for ( int i = 60 ; i < 65 ; i++ ) {
      cabling[i] = 55 + i ;
   }}
}
