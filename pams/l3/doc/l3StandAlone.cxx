#ifndef __CINT__
#include "Rtypes.h"
#include "St_XDFFile.h"
#include "St_DataSet.h"
#include "St_Module.h"
#include "St_Table.h"
#endif

// Global top level directory
Int_t STAFCV_OK  = 1;//  alias/create STAFCV_OK  1
Int_t STAFCV_BAD = 0;//  alias/create STAFCV_BAD 0
Int_t staf_status;

//

void wait() {
   char buffer[100];
   printf ( " Type any key to continue \n " ) ;
   scanf ( "%s", buffer ) ;
}

void Load() {
   gSystem->Load("St_base");
   gSystem->Load("St_Tables");
   gSystem->Load("libmsg");
   gSystem->Load("libtls");
   gSystem->Load("St_tpc");
   gSystem->Load("St_l3");
}
//
//
void runL3() {
   Load();
   int const maxHits   = 10000 ;
   int const maxBuffer = 100000 ;

   l3Geom = new St_l3Geom ("l3Geom",1 ) ;
   l3Geom_st    *l3GeomT = l3Geom->GetTable();
   l3GeomT->sector = 1 ;
//
   l3Hit   = new St_sl3Buffer("l3Hit",maxBuffer);
   l3Track = new St_sl3Buffer("l3Track",maxBuffer);
   tpHit   = new St_tcl_tphit("tpHit", maxHits ) ;
   tpHitC  = new St_tcl_tphit("tpHitC", maxHits ) ;
   tcl_tphit_st  *tpHitT = tpHit->GetTable();
   table_head_st *tpHitH = tpHit->GetHeader();
   tcl_tphit_st  *tpHitCT= tpHitC->GetTable();
//
//   Read hits from ascii file
//
   FILE* datafile = fopen("TpcSectorOneForAuAuCentralHijing.txt", "r");
// FILE* datafile = fopen("test.txt", "r");
   if (datafile == NULL)
   {
      printf ( " \n Error opening input file \n " ) ;
      return ;
   }
//
   float toDeg = 180./acos(-1.);
   float x, y, z ;
   int   i, row ;
   int   counter = 0 ;
   for ( i=0 ; i<23000 ; i++ )
   {
      if ( fscanf ( datafile, "%f %f %f %d", &x,&y,&z,&row) == EOF ) break ;
//
//    Check row
//
//    printf ( " row x y z %d %f %f %f \n ", row, x, y, z ) ;
//
//    Try to get rid of crazy hits
//
      float phi = atan2(y,x);
      phi *= toDeg ;
      if ( phi < 0 ) phi += 360 ;
      if ( phi <  45 ) continue ;
      if ( phi >  75 ) continue ;
//
      tpHitT[counter].row = 100 + row ;
      tpHitT[counter].x   = x ;
      tpHitT[counter].y   = y ;
      tpHitT[counter].z   = z ;
      tpHitT[counter].dx  = 0.2 ;
      tpHitT[counter].dy  = 0.2 ;
      tpHitT[counter].dz  = 0.2 ;
      counter++ ;
   }
   tpHitH->nok = counter ;

   
   //
   Int_t result = tpcHit2L3Hit ( l3Geom, tpHit, l3Hit ) ;
   printf ( " tpcHit2L3Hit: ended with status %d \n ", result ) ;
   result = l3Hit2TpcHit ( l3Geom, l3Hit, tpHitC ) ;
   printf ( " l3Hit2tpcHit: ended with status %d \n ", result ) ;

//
   int nBad = 0 ;
   for ( int i = 0 ; i < counter ; i++ ) {
  //  printf ( " Hit1: row x y z %d %f %f %f\n",tpHitT[i].row,tpHitT[i].x,tpHitT[i].y,tpHitT[i].z);
 //   printf ( " Hit2: row x y z %d %f %f %f\n",tpHitCT[i].row,tpHitCT[i].x,tpHitCT[i].y,tpHitCT[i].z);

      if ( fabs(tpHitT[i].x-tpHitCT[i].x) > 0.03 ) {
         printf ( " bad x hit1 %f hit2 %f \n",tpHitT[i].x, tpHitCT[i].x ) ;
         nBad++ ;
         continue ;
      }
      if ( fabs(tpHitT[i].y-tpHitCT[i].y) > 0.03 ) {
         printf ( " bad y hit1 %f hit2 %f \n",tpHitT[i].y, tpHitCT[i].y ) ;
         nBad++ ;
         continue ;
      }
      if ( fabs(tpHitT[i].z-tpHitCT[i].z) > 0.10 ) {
         printf ( " bad z hit1 %f hit2 %f \n",tpHitT[i].z, tpHitCT[i].z ) ;
         nBad++ ;
         continue ;
      }
   }
   printf ( " nBad hits %d\n", nBad ) ;
//
   sl3Para = new St_sl3TpcPara("sl3Para",1);
   sl3TpcPara_st *para  = sl3Para->GetTable();
   table_head_st *paraH = sl3Para->GetHeader();
//
//   Set para values
//
   para->infoLevel   =  5 ;
   para[0].FirstSector =  1 ;
   para[0].LastSector  =  2 ;
   para[0].InnerMostRow       =   1  ;
   para[0].OuterMostRow       =  45  ;
   para[0].startRow           =  45  ;
   para[0].NPrimaryLoops      =   1  ;
   para[0].NSecondaryLoops    =   0  ;
   para[0].ErrorScaleSz       =   1. ;
   para[0].ErrorScaleXy       =   1. ;
   para[0].Phimin             =  75./toDeg ;
   para[0].Phimax             = 105./toDeg ;
   para[0].Etamin             =  0.0 ;
   para[0].Etamax             =  2.2 ;
   para[0].PhiSlices          =  10  ;
   para[0].EtaSlices          =  40  ;
   para[0].BField             = 0.5  ;
   para[0].SFitSz             =   1  ;
   para[0].SPhiClosed         =   0  ;
   para[0].SDPhiLimit         = 0.08 ;
   para[0].SDEtaLimit         = 0.08 ;
   para[0].SChi2Cut           = 200.0 ;
   para[0].SGoodChi2          =  20.0 ;
   para[0].SChi2TrackCut      = 200.0 ;
   para[0].SGoodDistance      = 50. ;
   para[0].SMinimumHitsPerSegment =   2 ;
   para[0].SMinimumHitsPerTrack   =   5 ;
   para[0].SMaxSearchPadrowsTrack =   3 ;
   para[0].SMaxSearchPadrowsSegment = 1 ;
   para[0].MergePrimaries         =   1 ;
   para[0].NumberOfPsiSlices      =  40 ;
   para[0].NumberOfTanLSlices     =  10 ;
   para[0].MinSlicePsi            =  0.0 ;
   para[0].MaxSlicePsi            = 360. ;
   para[0].MinSliceTanL           = -2.0 ;
   para[0].MaxSliceTanL           =  2.0 ;
   para[0].SDPsiMaxMerge          =  0.03 ;
   para[0].SDTanlMaxMerge         =  0.01 ;
//
   printf ( " infoLevel %d \n", para->infoLevel ) ;
   result = sl3Tpc ( sl3Para, l3Hit, l3Track ) ;
//
   printf ( " ******************************************* \n" ) ;
   printf ( " if you get 7 bad hits and ~383 tracks  \n" ) ;
   printf ( "      the test has been successful      \n" ) ;
   printf ( " ******************************************* \n" ) ;
   printf ( " if that's the case: Congratulations!!! \n" ) ;
   printf ( " if not, keep trying, perseverance pays off \n" ) ;
   printf ( " ******************************************* \n" ) ;
}
