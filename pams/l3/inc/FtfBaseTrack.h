//:>----------------------------------------------------------------------
//: FILE:      FtfBaseTrack.h
//: HISTORY:
//:           28jan2000  start writting changes
//:                      innerMostRow and outerMostRow added
//:            9mar2000  ppy    tracklength moved to FtfBaseTrack
//:           17mar2000  ppy    add methods to extrapolate tracks 
//:           19apr2000  cs ppy add nDedx for dEdx calculations   
//:>----------------------------------------------------------------------
#ifndef  FTFBASETRACK
#define  FTFBASETRACK
#include "FtfGeneral.h"
#include "FtfBaseHit.h"
#include "FtfPara.h"

#ifdef SL3ROOT
#include "Rtypes.h"
#include "TObject.h"
#else
#define ClassDef(a,b)
#endif
//
//    Base Track class
//
class FtfBaseTrack 
#ifdef SL3ROOT
: public TObject
#endif
{ 
      
public:
    FtfBaseTrack ( ) ;
    virtual ~FtfBaseTrack(){};
    void        *firstHit;// First hit belonging to track
    void        *lastHit ;// Last  hit belonging to track
    void        *currentHit ;
    int         fitHelix   (  ) ;
    int         fitCircle  (  ) ;
    int         fitLine    (  ) ;
    FtfBaseHit* getCurrentHit ( ) { return (FtfBaseHit *)currentHit ; } ;
    FtfPara*    getPara()         { return (FtfPara *)para ; } ;
    int         getErrorsCircleFit ( double a, double b, double r ) ;


    double   arcLength       ( double x1, double y1, double x2, double y2 ) ;
    Ftf3DHit closestApproach ( double xBeam, double yBeam ) ;
    Ftf3DHit extraRadius     ( double r ) ;
    int      extraRCyl       ( double &r, double &phi, double &z,
                               double &rc, double &xc, double &yc ) ;
    int      intersectorZLine ( double a, double b, Ftf3DHit& cross ) ;
    Ftf3DHit getClosest      ( double xBeam, double yBeam,
	                       double &rc, double &xc, double &yc ) ;
    int      getClosest      ( double xBeam, double yBeam,
	                       double rc, double xc, double yc,
	                       double &xClosest, double &yClosest ) ;

    void     updateToRadius  ( double r ) ;
    void     updateToClosestApproach ( double xBeam, double yBeam ) ;
    int      phiRotate       ( double deltaPhi ) ;

    inline virtual   void startLoop( ){ currentHit = firstHit ; } ;
    inline virtual   void nextHit  ( ) = 0 ; 
    inline virtual   int  done     ( ) { return currentHit != 0 ; } ;
    void       Print       ( int level ) ;
	  
    double     bField ; 

    int       id     ;  // primary key 
    short     flag   ;  // Primaries flag=1, Secondaries flag=0      
    char      innerMostRow ;
    char      outerMostRow ;
    short     nHits  ;  // Number of points assigned to that track
    short     nDedx  ;  // Number of points used for dEdx
    short     q  ;      // charge 
    double     chi2[2];  // chi squared of the momentum fit 
    double     dedx;     // dE/dx information 
    double     pt  ;     // pt (transverse momentum) at (r,phi,z) 
    double     phi0;     // azimuthal angle of the first point 
    double     psi ;     // azimuthal angle of the momentum at (r,.. 
    double     r0  ;     // r (in cyl. coord.) for the first point 
    double     tanl;     // tg of the dip angle at (r,phi,z) 
    double     z0  ;     // z coordinate of the first point 
    double     length ;
    double     dpt ;
    double     dpsi;
    double     dz0 ;
    double     eta ;
    double     dtanl ;

    void     *para  ;    // Parameters pointer     
    ClassDef(FtfBaseTrack,1)

   } ;
#endif

