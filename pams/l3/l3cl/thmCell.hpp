// Type definitions

#include <math.h>
#include "thmList.hpp"
#include "g2t_tpc_hit.h"
#include "tcl_tphit.h"

class thmPoint ;

typedef thmList <thmPoint*>      pointList ;

typedef G2T_TPC_HIT_ST*  mHitElt;
typedef TCL_TPHIT_ST*    tHitElt;

const double pi=3.14159265358979;
const double twoPi = 2. * pi ;
double   toDeg = 180./acos(-1.);

//int   nRadialDivs = 10;     // divisions
//int   nPhiDivs    = 10;
//int   nEtaDivs    = 10;


class thmGeometry {

public:
   int   nRadialDivs  ;     // divisions
   int   nPhiDivs     ;
   int   nEtaDivs     ;
   int   maxCells     ;
   int   innerMostRow ;
   int   outerMostRow ;
   float phiMin ;
   float phiMax ;
   float etaMin ;
   float etaMax ;
   float phiStep ;
   float etaStep ;
//
   long Set ( THM_CTRL_ST* ctrl ) {
   //
   //   
      if ( ctrl->innerMostRow > ctrl->outerMostRow ) {
         cout<<" thm: Inner Most Row larger that OuterMostRow "<<endl;
         return 1 ;
      }
   //
   // 
      if ( ctrl->phiMin > ctrl->phiMax ) {
         cout<<" thm: phiMin > PhiMax "<<endl;
         return 1 ;
      }
   //
   //   
      if ( ctrl->etaMin > ctrl->etaMax ) {
         cout<<" thm: phiMin > PhiMax "<<endl;
         return 1 ;
      }
   //
   //   
      if ( ctrl->nRadialDivs < 10 ) {
         cout<<" thm: nRadialDiv < 10 "<<endl;
         return 1 ;
      }
   //
   //   
      if ( ctrl->nPhiDivs < 2 ) {
         cout<<" thm: nPhiDiv < 2 "<<endl;
         return 1 ;
      }
   //
   //   
      if ( ctrl->nEtaDivs < 2 ) {
         cout<<" thm: nEtaDiv < 2 "<<endl;
         return 1 ;
      }
   //
   //   Set general variables for thmCell
   //
      nRadialDivs  = ctrl->nRadialDivs;
      nPhiDivs     = ctrl->nPhiDivs;
      nEtaDivs     = ctrl->nEtaDivs;
      maxCells     = nRadialDivs * nPhiDivs * nEtaDivs ;
      innerMostRow = ctrl->innerMostRow ;
      outerMostRow = ctrl->outerMostRow ;
      phiMin       = ctrl->phiMin ;
      phiMax       = ctrl->phiMax ;
      etaMin       = ctrl->etaMin ;
      etaMax       = ctrl->etaMax ;
   //
   //   Check steps
   //
      phiStep = ( ctrl->phiMax - ctrl->phiMin ) / ctrl->nPhiDivs ;
      etaStep = ( ctrl->etaMax - ctrl->etaMin ) / ctrl->nEtaDivs ;
   //
      return 0 ;

   }

};


class thmCell
{
public:
  int ir;
  int ip;
  int ie;
  pointList mcHits ;

  thmCell ( ) { } ;

  int index(int R,int P,int E, thmGeometry& geo) {
     return (R+geo.nRadialDivs*(P+(geo.nPhiDivs*E)));
  }


  thmCell(int i, thmGeometry& geo) {
    ir = i % geo.nRadialDivs;
    ip = (i / geo.nRadialDivs) % geo.nPhiDivs;
    ie = (i / (geo.nRadialDivs*geo.nPhiDivs));
  }

  void init(int i, thmGeometry& geo) {
    ir = i % geo.nRadialDivs;
    ip = (i / geo.nRadialDivs) % geo.nPhiDivs;
    ie = (i / (geo.nRadialDivs*geo.nPhiDivs));
  }

  thmCell(int R,int P, int E) {
    ir=R; ip=P; ie=E;
  }

  int nn3(int n, thmGeometry& geo) { //Nearest neighbours in 3D
    int jr, jp, je, dr, dp, de;
    if (n==0) n=13;
    else if (n<=13) n--;
    dr=(n%3)-1;
    dp=((n/3)%3)-1;
    de=(n/9)-1;  
    jr=ir+dr; jp=ip+dp; je=ie+de;
    //    dataPoint iPt,jPt,dPt;
    //    iPt.init(ir,ip,ie);
    //    jPt.init(jr,jp,je);
    //    dPt.init(dr,dp,de);
    //    cout << "nn called; n=" << n << dPt <<" ";
    //    cout << iPt << " " <<jPt <<"\n" << flush;
    if (jr<0 || jr> geo.nRadialDivs || jp<0 || jp>geo.nPhiDivs || je<0 || je>geo.nEtaDivs) {
      return -1;
    } else {
      return index (jr, jp, je, geo);
    }
  }

  int nn(int n, thmGeometry& geo) { //Nearest neighbours in 2D
    int jr, jp, je, dr, dp, de;
    if (n==0) n=4;
    else if (n<=4) n--;
    dr = 0; // confining ourselves to one row
    dp=(n%3)-1;
    de=(n/3)-1;
    jr=ir+dr; jp=ip+dp; je=ie+de;
    //    dataPoint iPt,jPt,dPt;
    //    iPt.init(ir,ip,ie);
    //    jPt.init(jr,jp,je);
    //    dPt.init(dr,dp,de);
    //    cout << "nn called; n=" << n << dPt <<" ";
    //    cout << iPt << " " <<jPt <<"\n" << flush;
    if (jr<0 || jr> geo.nRadialDivs || jp<0 || jp>geo.nPhiDivs || je<0 || je>geo.nEtaDivs) {
      return -1;
    } else {
      return index (jr, jp, je, geo);
    }
  }

};

class thmPoint {

public:
   float phi ;
   float r   ;
   thmCell* pCell ;
   void*    pTable ;

   int Set ( int row, float x, float y, float z, thmGeometry& geo ) {
//
//   Check row number
//
      if (row < geo.innerMostRow-1 || row >= geo.outerMostRow ) return -1 ;
      r = (float)sqrt(x*x+y*y);
      phi = (float)atan2(y,x);
      if ( phi < 0 ) phi += (float)twoPi ;
      float eta = -(float)(log(tan(atan2(r,z)/2))) ; 
//
//   Calculate indexes
//
      int phii = int((toDeg*phi-geo.phiMin)/geo.phiStep); 
      int etai = int((eta-geo.etaMin)/geo.etaStep);
      int i    = row+geo.nRadialDivs*(phii+(geo.nPhiDivs*etai));  // Cell::index()
      if ( i < 0 || i >= geo.maxCells ) return -1 ;
      return i;
   };
   
};














