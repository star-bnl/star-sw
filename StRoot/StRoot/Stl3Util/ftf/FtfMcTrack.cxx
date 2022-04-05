//:>------------------------------------------------------------------
//: FILE:       FtfMcTrack.cxx
//: HISTORY:
//:            1may2000 new version
//:           10may2000 ppy add set    
//:
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfMcTrack
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@rice.edu
//:>------------------------------------------------------------------
#include "Stl3Util/ftf/FtfMcTrack.h"

void FtfMcTrack::set ( int _id, float _r0, float _z0, float _phi0,
                       float _pt, float _tanl, float _psi, 
		       int _q, int _nHits ) {
   id    = _id ;
   r0    = _r0 ;
   z0    = _z0 ;
   phi0  = _phi0 ;
   pt    = _pt ;
   tanl  = _tanl ;
   psi   = _psi ;
   q     = _q ;
   nHits = _nHits ;
}

//*****************************************************************
//    set Primary
//*****************************************************************
void FtfMcTrack::setPrimary ( short qIn, float ptIn, float eta, float psiIn ) {
   q    = qIn ;
   float theta    = 2 * atan(exp(-eta )) ;
   tanl = 1./ tan(theta);
   pt   = ptIn ;
   psi  = psiIn ;
   r0   = 0. ;
   phi0 = 0. ;
   z0   = 0. ;
} 
//*****************************************************************
//   set Random primary
//*****************************************************************
void FtfMcTrack::setRandomPrimary ( float ptMin,  float ptMax, 
            float etaMin, float etaMax, float psiMin, float psiMax,
            float zVert ) {
   pt   = ptMin  + (ptMax  - ptMin  ) * (float)rand() / (float)RAND_MAX ;
   double eta  = etaMin + (etaMax - etaMin ) * (float)rand() / (float)RAND_MAX ; 
   double theta    = 2 * atan(exp(-eta )) ;
   tanl = 1./ tan(theta);
   psi  = psiMin + (psiMax - psiMin ) * (float)rand() / (float)RAND_MAX ; 
   q    = 1 ;
   r0   = 0. ;
   phi0 = 0. ;
   z0   = zVert ;
   if ( (float)rand()/(float)RAND_MAX > 0.5 ) q = -1 ;
}
