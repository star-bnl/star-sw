//:>------------------------------------------------------------------
//: FILE:       FtfPara.cxx
//: HISTORY:
//:             28oct1996 version 1.00
//:              7dec1998 ppy variable names changed to C++ style
//:              3jun1999 ppy add fillTracks flag
//:             11aug1999 ppy add vertexContrainedFit variable
//:             23aug1999 ppy add Root option
//:             11feb2000 ppy add maxTime 
//:             13mar2000 ppy add default value for rowEnd
//:                       
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfPara
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------

#include "Stl3Util/ftf/FtfPara.h"
#include "Stl3Util/ftf/FtfGeneral.h"
//#include "Stl3Util/ftf/FtfFinder.h"

#include <string.h>


void FtfPara::read ( char* inputFile ) {

   FILE* dataFile = fopen( inputFile, "r");
   if (dataFile == NULL) {
      printf ( "FtfPara::write: Error opening input file %s \n", inputFile ) ;
      return ;
   }

   char* name = new char[100] ;
   while ( 1 ) {
      if ( fscanf ( dataFile, "%s", name ) == EOF ) break ;
      if ( !strncmp(name,"infoLevel"       ,8) ) { 
         fscanf ( dataFile, "%d", &infoLevel ) ;
         continue ;
      }
      if ( !strncmp(name,"segmentRowSearch",8) ) {
         fscanf ( dataFile, "%d", &segmentRowSearchRange ) ;
         continue ;
      }
      if ( !strncmp(name,"trackRowSearch",  8) ) {
         fscanf ( dataFile, "%d", &trackRowSearchRange ) ;
         continue ;
      }
      if ( !strncmp(name,"getErrors     ",  8) ) {
         fscanf ( dataFile, "%d", &getErrors      ) ;
         continue ;
      }
      if ( !strncmp(name,"fillTracks    ",  8) ) {
         fscanf ( dataFile, "%d", &fillTracks     ) ;
         continue ;
      }
      if ( !strncmp(name,"ghostFlag     ",  8) ) {
         fscanf ( dataFile, "%d", &ghostFlag      ) ;
         continue ;
      }
      if ( !strncmp(name,"goBackwards   ",  8) ) {
         fscanf ( dataFile, "%d", &goBackwards    ) ;
         continue ;
      }
      if ( !strncmp(name,"mergePrimaries",  8) ) {
         fscanf ( dataFile, "%d", &mergePrimaries ) ;
         continue ;
      }
      if ( !strncmp(name,"minHitsPerTrack", 8) ) {
         fscanf ( dataFile, "%d", &minHitsPerTrack ) ;
         continue ;
      }
      if ( !strncmp(name,"modRow",          6) ) {
         fscanf ( dataFile, "%d", &modRow          ) ;
         continue ;
      }
      if ( !strncmp(name,"nHitsForSegment", 8) ) {
         fscanf ( dataFile, "%d", &nHitsForSegment ) ;
         continue ;
      }
      if ( !strncmp(name,"minHitsForFit",   8) ) {
         fscanf ( dataFile, "%d", &minHitsForFit   ) ;
         continue ;
      }
      if ( !strncmp(name,"nEtaTrack",   8) ) {
          fscanf ( dataFile, "%i", &nEtaTrack     ) ;
          continue;
      }
      if ( !strncmp(name,"nEta",   4) ) {
          fscanf ( dataFile, "%d", &nEta          ) ;
          continue ;
      }
      if ( !strncmp(name,"nPhiTrack",   8) ) {
         fscanf ( dataFile, "%d", &nPhiTrack     ) ;
         continue ;
      }
      if ( !strncmp(name,"nPhi",   4) ) {
         fscanf ( dataFile, "%d", &nPhi          ) ;
         continue ;
      }
      if ( !strncmp(name,"detaMerge    ",   8) ) {
         fscanf ( dataFile, "%e", &detaMerge     ) ;
         continue ;
      }
      if ( !strncmp(name,"deta         ",   4) ) {
         fscanf ( dataFile, "%e", &deta          ) ;
         continue ;
      }  
      if ( !strncmp(name,"dphiMerge    ",   8) ) {
         fscanf ( dataFile, "%e", &dphiMerge     ) ;
         continue ;
      }
      if ( !strncmp(name,"dphi         ",   4) ) {
         fscanf ( dataFile, "%e", &dphi          ) ;
         continue ;
      }  
      if ( !strncmp(name,"etaMinTrack  ",   8) ) {
         fscanf ( dataFile, "%e", &etaMinTrack   ) ;
         continue ;
      }
      if ( !strncmp(name,"etaMin",   6) ) {
         fscanf ( dataFile, "%e", &etaMin        ) ;
         continue ;
      }  
      if ( !strncmp(name,"etaMaxTrack  ",   8) ) {
         fscanf ( dataFile, "%e", &etaMaxTrack   ) ;
         continue ;
      }
      if ( !strncmp(name,"etaMax       ",   6) ) {
         fscanf ( dataFile, "%e", &etaMax        ) ;
         continue ;
      }  
      if ( !strncmp(name,"phiMinTrack  ",   8) ) {
         fscanf ( dataFile, "%e", &phiMinTrack   ) ;
         continue ;
      }
      if ( !strncmp(name,"phiMin       ",   6) ) {
         fscanf ( dataFile, "%e", &phiMin        ) ;
         continue ;
      }  
      if ( !strncmp(name,"phiMaxTrack  ",   8) ) {
         fscanf ( dataFile, "%e", &phiMaxTrack   ) ;
         continue ;
      }
      if ( !strncmp(name,"phiMax       ",   6) ) {
         fscanf ( dataFile, "%e", &phiMax        ) ;
         continue ;
      }  
      if ( !strncmp(name,"phiShift     ",   8) ) {
         fscanf ( dataFile, "%e", &phiShift      ) ;
         continue ;
      }  
      if ( !strncmp(name,"distanceMerge",   8) ) {
         fscanf ( dataFile, "%e", &distanceMerge ) ;
         continue ;
      }  
      if ( !strncmp(name,"nPrimaryPasses",  8) ) {
         fscanf ( dataFile, "%d", &nPrimaryPasses ) ;
         continue ;
      }  
      if ( !strncmp(name,"nSecondary",  8) ) {
         fscanf ( dataFile, "%d", &nSecondaryPasses ) ;
         continue ;
      }  
      if ( !strncmp(name,"vertexConstrainedFit",  8) ) {
         fscanf ( dataFile, "%d", &vertexConstrainedFit ) ;
         continue ;
      }  
      if ( !strncmp(name,"parameterLocation",  8) ) {
         fscanf ( dataFile, "%d", &parameterLocation ) ;
         continue ;
      }  
      if ( !strncmp(name,"rowInnerMost",  8) ) {
         fscanf ( dataFile, "%d", &rowInnerMost ) ;
         continue ;
      }  
      if ( !strncmp(name,"rowOuterMost",  8) ) {
         fscanf ( dataFile, "%d", &rowOuterMost ) ;
         continue ;
      }  
      if ( !strncmp(name,"rowStart",         8) ) {
         fscanf ( dataFile, "%d", &rowStart ) ;
         continue ;
      }  
      if ( !strncmp(name,"rowEnd",           6) ) {
         fscanf ( dataFile, "%d", &rowEnd ) ;
         continue ;
      }  
      if ( !strncmp(name,"szFitFlag",        8) ) {
         fscanf ( dataFile, "%d", &szFitFlag ) ;
         continue ;
      }  
      if ( !strncmp(name,"bField",       6) ) {
         fscanf ( dataFile, "%e", &bField ) ;
         continue ;
      }  
      if ( !strncmp(name,"maxChi2Primary",       12) ) {
         fscanf ( dataFile, "%e", &maxChi2Primary ) ;
         continue ;
      }  
      if ( !strncmp(name,"hitChi2Cut",           8) ) {
         fscanf ( dataFile, "%e", &hitChi2Cut ) ;
         continue ;
      }  
      if ( !strncmp(name,"goodHitChi2",           8) ) {
         fscanf ( dataFile, "%e", &goodHitChi2 ) ;
         continue ;
      }  
      if ( !strncmp(name,"trackChi2Cut",           8) ) {
         fscanf ( dataFile, "%e", &trackChi2Cut ) ;
         continue ;
      } 
      if ( !strncmp(name,"goodDistance",           8) ) {
         fscanf ( dataFile, "%e", &goodDistance ) ;
         continue ;
      } 
      if ( !strncmp(name,"ptMinHelixFit",           8) ) {
         fscanf ( dataFile, "%e", &ptMinHelixFit ) ;
         continue ;
      } 
      if ( !strncmp(name,"maxDistanceSegment",   15) ) {
         fscanf ( dataFile, "%e", &maxDistanceSegment ) ;
         continue ;
      } 
      if ( !strncmp(name,"xyErrorScale",   10) ) {
         fscanf ( dataFile, "%e", &xyErrorScale ) ;
         continue ;
      } 
      if ( !strncmp(name,"szErrorScale",   10) ) {
         fscanf ( dataFile, "%e", &szErrorScale ) ;
         continue ;
      } 
      if ( !strncmp(name,"xVertex",   7) ) {
         fscanf ( dataFile, "%e", &xVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"yVertex",   7) ) {
         fscanf ( dataFile, "%e", &yVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"zVertex",   7) ) {
         fscanf ( dataFile, "%e", &zVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"dxVertex",   8) ) {
         fscanf ( dataFile, "%e", &dxVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"dyVertex",   8) ) {
         fscanf ( dataFile, "%e", &dyVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"xyWeightVertex",   8) ) {
         fscanf ( dataFile, "%e", &xyWeightVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"phiVertex",   8) ) {
         fscanf ( dataFile, "%e", &phiVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"rVertex",   7) ) {
         fscanf ( dataFile, "%e", &rVertex ) ;
         continue ;
      } 
      if ( !strncmp(name,"maxTime",   7) ) {
         fscanf ( dataFile, "%e", &maxTime ) ;
         continue ;
      } 
      printf ( "FtfPara::read: parameter %s not found \n", name ) ;
      float variable ; 
      fscanf ( dataFile, "%e", &variable ) ;

   }

   fclose ( dataFile ) ;

}

void FtfPara::write ( char* outputFile ) {
   FILE* dataFile = fopen( outputFile, "w");
   if (dataFile == NULL) {
      printf ( "FtfPara::write: Error opening output file %s \n ", outputFile ) ;
      return ;
   }
   write ( dataFile ) ;
   fclose ( dataFile ) ;
}

void FtfPara::write ( FILE* dataFile ) {

   fprintf ( dataFile, "infoLevel            %10d \n", infoLevel ) ;
   fprintf ( dataFile, "segmentRowSearch     %10d  \n", segmentRowSearchRange ) ;
   fprintf ( dataFile, "trackRowSearch       %10d  \n", trackRowSearchRange ) ;
   fprintf ( dataFile, "getErrors            %10d  \n", getErrors ) ;
   fprintf ( dataFile, "fillTracks           %10d  \n", fillTracks ) ;
   fprintf ( dataFile, "ghostFlag            %10d  \n", ghostFlag ) ;
   fprintf ( dataFile, "goBackwards          %10d  \n", goBackwards ) ;
   fprintf ( dataFile, "mergePrimaries       %10d  \n", mergePrimaries ) ;
   fprintf ( dataFile, "minHitsPerTrack      %10d  \n", minHitsPerTrack ) ;
   fprintf ( dataFile, "modRow               %10d  \n", modRow ) ;
   fprintf ( dataFile, "nHitsForSegment      %10d  \n", nHitsForSegment ) ;
// fprintf ( dataFile, "minHitsForFit        %10d  \n", minHitsForFit   ) ;
   fprintf ( dataFile, "nEta                 %10d  \n", nEta            ) ;
   fprintf ( dataFile, "nPhi                 %10d  \n", nPhi            ) ;
   fprintf ( dataFile, "deta                 %10.2e\n", deta         ) ;
   fprintf ( dataFile, "dphi                 %10.2e\n", dphi         ) ;
   fprintf ( dataFile, "etaMin               %10.2e\n", etaMin        ) ;
   fprintf ( dataFile, "etaMax               %10.2e\n", etaMax        ) ;
   fprintf ( dataFile, "phiMin               %10.2e\n", phiMin        ) ;
   fprintf ( dataFile, "phiMax               %10.2e\n", phiMax        ) ;
   fprintf ( dataFile, "phiShift             %10.2e\n", phiShift   ) ;
   fprintf ( dataFile, "detaMerge            %10.2e\n", detaMerge    ) ;
   fprintf ( dataFile, "distanceMerge        %10.2e\n", distanceMerge ) ;
   fprintf ( dataFile, "nEtaTrack            %10d  \n", nEtaTrack       ) ;
   fprintf ( dataFile, "nPhiTrack            %10d  \n", nPhiTrack       ) ;
   fprintf ( dataFile, "etaMinTrack          %10.2e\n", etaMinTrack   ) ;
   fprintf ( dataFile, "etaMaxTrack          %10.2e\n", etaMaxTrack   ) ;
   fprintf ( dataFile, "phiMinTrack          %10.2e\n", phiMinTrack   ) ;
   fprintf ( dataFile, "phiMaxTrack          %10.2e\n", phiMaxTrack   ) ;
   fprintf ( dataFile, "nPrimaryPasses       %10d  \n", nPrimaryPasses  ) ;
   fprintf ( dataFile, "nSecondaryPasses     %10d  \n", nSecondaryPasses  ) ;
   fprintf ( dataFile, "vertexConstrainedFit %10d  \n", vertexConstrainedFit ) ;
   fprintf ( dataFile, "parameterLocation    %10d  \n", parameterLocation ) ;
   fprintf ( dataFile, "rowInnerMost         %10d  \n", rowInnerMost      ) ;
   fprintf ( dataFile, "rowOuterMost         %10d  \n", rowOuterMost      ) ;
   fprintf ( dataFile, "rowStart             %10d  \n", rowStart          ) ;
   fprintf ( dataFile, "rowEnd               %10d  \n", rowEnd            ) ;
   fprintf ( dataFile, "szFitFlag            %10d  \n", szFitFlag         ) ;
   fprintf ( dataFile, "maxChi2Primary       %10.2e\n", maxChi2Primary    ) ;
   fprintf ( dataFile, "bField               %10.2e\n", bField            ) ;
   fprintf ( dataFile, "hitChi2Cut           %10.2e\n", hitChi2Cut ) ;
   fprintf ( dataFile, "goodHitChi2          %10.2e\n", goodHitChi2 ) ;
   fprintf ( dataFile, "trackChi2Cut         %10.2e\n", trackChi2Cut ) ;
   fprintf ( dataFile, "goodDistance         %10.2e\n", goodDistance ) ;
   fprintf ( dataFile, "ptMinHelixFit        %10.2e\n", ptMinHelixFit ) ;
   fprintf ( dataFile, "maxDistanceSegment   %10.2e\n", maxDistanceSegment ) ;
   fprintf ( dataFile, "xyErrorScale         %10.2e\n", xyErrorScale       ) ;
   fprintf ( dataFile, "szErrorScale         %10.2e\n", szErrorScale       ) ;
   fprintf ( dataFile, "xVertex              %10.2e\n", xVertex            ) ;
   fprintf ( dataFile, "yVertex              %10.2e\n", yVertex            ) ;
   fprintf ( dataFile, "dxVertex             %10.2e\n", dxVertex           ) ;
   fprintf ( dataFile, "dyVertex             %10.2e\n", dyVertex           ) ;
   fprintf ( dataFile, "zVertex              %10.2e\n", zVertex            ) ;
   fprintf ( dataFile, "xyWeightVertex       %10.2e\n", xyWeightVertex     ) ;
   fprintf ( dataFile, "phiVertex            %10.2e\n", phiVertex          ) ;
   fprintf ( dataFile, "rVertex              %10.2e\n", rVertex            ) ;
   fprintf ( dataFile, "maxTime              %10.2e\n", maxTime            ) ;
}

void FtfPara::setDefaults (void)
{
/*  Define cuts - this should be obsolete */

   modRow          = 1    ;
   infoLevel       = 0 ;
   hitChi2Cut      = 500.F  ;
   goodHitChi2     = 100.F ;
   trackChi2Cut    = 250.F ;
   maxChi2Primary  = 0. ;
   segmentRowSearchRange = 1 ;
   trackRowSearchRange   = 3 ;
   dEdx              = 0     ;
   dEdxNTruncate     = 20    ;
   minHitsForDedx    = 15    ;
   dphi              = 0.10F * modRow ;
   deta              = 0.10F * modRow ;
   dphiMerge         = 0.02F  ;
   detaMerge         = 0.02F  ;
   distanceMerge     = 2. ;
   etaMin            = -2.5F  ;
   etaMinTrack       = -2.2F  ;
   etaMax            =  2.5F  ;
   etaMaxTrack       =  2.2F  ;
   eventReset        =  1     ;
   getErrors         =  0     ;
   fillTracks        =  1     ;
   ghostFlag         =  0     ;
   goBackwards       =  0     ;
   goodDistance      =  1.F * modRow ;
   init              =  0 ;
   mergePrimaries    =  1    ;
   parameterLocation =  1    ;
   phiMin            =  (float)(-0.000001/toDeg)  ;
   phiMinTrack       =  (float)(-0.000001/toDeg)  ;
   phiMax            = (float)(360.2/toDeg)  ;
   phiMaxTrack       = (float)(360.2/toDeg)  ;
   maxDistanceSegment = 100.F * modRow ;
   minHitsPerTrack   = 5      ;
   nHitsForSegment   = 2      ;
   nEta              = 60     ;
   nEtaTrack         = 60     ;
   nPhi              = 20     ;
   nPhiTrack         = 60     ;
   nPrimaryPasses    = 1      ;
   nSecondaryPasses  = 0      ;
   vertexConstrainedFit = 0 ;
   rowInnerMost      = 1      ;
   rowOuterMost      = 45     ;
   rowStart          = 45     ;
   rowEnd            =  1     ;
   segmentMaxAngle   = 10.F/toDeg ;
   szFitFlag         = 1      ;
   xyErrorScale      = 1.0F   ;
   szErrorScale      = 1.0F   ;
   bField            = 0.0F   ;
   phiShift          = 0.0    ;
   zMax              = 210.   ;
   
   ptMinHelixFit     = 0.F  ;
   rVertex           = 0.F    ;
   xVertex           = 0.F    ;
   yVertex           = 0.F    ;
   zVertex           = 0.F    ;
   dxVertex          = 0.005F ;
   dyVertex          = 0.005F ;
   phiVertex         = 0.F    ;
   maxTime           = 1.e18 ; // by default tracker can run as long as the age of the Universe

   return  ;
}
