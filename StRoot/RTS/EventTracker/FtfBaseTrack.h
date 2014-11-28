//:>----------------------------------------------------------------------
//: FILE:      FtfBaseTrack.h
//: HISTORY:
//:           28jan2000  start writting changes
//:                      innerMostRow and outerMostRow added
//:            9mar2000  ppy    tracklength moved to FtfBaseTrack
//:           17mar2000  ppy    add methods to extrapolate tracks 
//:           19apr2000  cs ppy add nDedx for dEdx calculations   
//:           10aug2000  ppy    remove bField, to be kept in one place only (in FtfPara para) 
//:>----------------------------------------------------------------------
#ifndef  FTFBASETRACK
#define  FTFBASETRACK
//#include "FtfGeneral.h"
#include "FtfBaseHit.h"
#include "FtfPara.h"


class FtfBaseTrack 
{ 
      
 public:
    FtfBaseTrack ( ) ;
    virtual ~FtfBaseTrack(){};
    void        *firstHit;// First hit belonging to track
    void        *lastHit ;// Last  hit belonging to track
    void        *currentHit ;
    int         fitHelix   (  ) ;
    int         refitHelix ( int mode, int modEqual, int rowMin, int rowMax ) ;
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
    int      intersectorZLine    ( double a, double b, 
				   Ftf3DHit& cross1, Ftf3DHit& cross2 ) ;
    int      intersectorZLine    ( double a, double b, Ftf3DHit& cross ) ;
    int      intersectorYCteLine ( double a, Ftf3DHit& cross ) ;
    Ftf3DHit getClosest      ( double xBeam, double yBeam,
	                       double &rc, double &xc, double &yc ) ;
    int      getClosest      ( double xBeam, double yBeam,
	                       double rc, double xc, double yc,
	                       double &xClosest, double &yClosest ) ;

    void     updateToRadius  ( double r ) ;
    void     updateToClosestApproach ( double xBeam, double yBeam, double rMax=10000. ) ;
    int      phiRotate       ( double deltaPhi ) ;
    // JB
    Ftf3DHit extrapolate2PathLength(double pathlength);
    double getRadius();
    double getXCenter();
    double getYCenter();
    double pathLength(double Rx, double Ry, double Rz, double Nx, double Ny, double Nz );
    
    inline virtual   void startLoop( ){ currentHit = firstHit ; } ;
    virtual   void nextHit  ( ) = 0 ; 
    inline virtual   int  done     ( ) { return currentHit != 0 ; } ;
    void       Print       ( int level ) ;
	  

    int       id     ;  // primary key 
    short     flag   ;  // Primaries flag=1, Secondaries flag=0      
    char      innerMostRow ;
    char      outerMostRow ;
    short     nHits  ;  // Number of points assigned to that track
    short     nDedx  ;  // Number of points used for dEdx
    short     q  ;      // charge 
    double    chi2[2];  // chi squared of the momentum fit 
    double    dedx;     // dE/dx information 
    double    pt  ;     // pt (transverse momentum) at (r,phi,z) 
    double    phi0;     // azimuthal angle of point where parameters are given 
    double    psi ;     // azimuthal angle of the momentum at (r,.. 
    double    r0  ;     // r (in cyl. coord.) for point where parameters given 
    double    tanl;     // tg of the dip angle at (r,phi,z) 
    double    z0  ;     // z coordinate of point where parameters are given 
    double    length ;
    double    dpt ;
    double    dpsi;
    double    dz0 ;
    double    eta ;
    double    dtanl ;

    void     *para  ;    // Parameters pointer     

    unsigned short CompressOver1(double,double);
    double DecompressOver1(unsigned short,double);

} ;

inline unsigned short FtfBaseTrack::CompressOver1(double v1,double v2) {
  // drop 10 bits of precision for 1 < v1/|v2| < 1024 (upper limit)
  double intermed = v1/fabs(v2);
  return (intermed < 1 ?  (unsigned short) (intermed*32768.) :
         (intermed > 1024. ? 65535 :
          32768 + (unsigned short) ((intermed - 1.)*32.)));
}
inline double FtfBaseTrack::DecompressOver1(unsigned short v1,double v2) {
  return fabs(v2)*(v1 <= 32768 ? ((double) v1)/32768. :
                                 ((double) (v1 - 32768))/32. + 1.);
}


#endif

