#ifndef G_COMPONENT_ANALYTIC_FIELD__H
#define G_COMPONENT_ANALYTIC_FIELD__H

#include <RQ_OBJECT.h>
#include "ComponentBase.hh"
#include "Medium.hh"
#include "GeometryBase.hh"
#include "GeometrySimple.hh"
#include <string.h>

//PARAMETERS SET AT THE TOP OF EACH GARFIELD SUBROUTINE
//These parameters are being declared globally
#define mxwire 2000
#define mxsw 200
#define mxmatt 10 
#define mx3d 100
#define mxpole 10
#define mxpstr 100
#define mxlist 1000
#define mxhist 200
#define mxcha mxlist/2
#define mxgrid 50
#define mxname 200
#define mxlun 30
#define mxclus 500
#define mxpair 2000
#define mxpart 10000
#define mxline 150
#define mxequt 50
#define mxfour 16 
#define mxrecl 10000
#define mxinch 2000
#define mxword 200
#define mxchar mxinch
#define mxins 1000
#define mxreg 500
#define mxcons -500
#define mxvar 500
#define mxalge 500
#define mxarg 100
#define mxmat 500
#define mxemat 100000
#define mxmdim 10
#define mxzero mxwire
#define mxstck 5
#define mxfpnt 1000
#define mxfpar 10
#define mxwkls 10
#define mxhlev 9
#define mxsubt 200
#define mxhlrl 860
#define mxdlvl 10
#define mxilvl 20 
#define mxdlin 500
#define mxfrac 13
#define mxbang 20 
#define mxbtab 25
#define mxexg 50 
#define mxiog 10 
#define mxcsg 200
#define mxoria 1000
#define mxshot 10
#define mxzpar 4*mxshot+2
#define mxmap 150000
#define mxeps 10 
#define mxwmap 5
#define mxsoli 5000
#define mxplan 50000
#define mxpoin 100000
#define mxedge 100
#define mxsbuf 10000
#define mxmca 50000
#define mxnbmc 60

/*Structure defining a complex number*/


 struct complex_{
    float real;
    float imag;
  } ;


extern "C" {
 //COMMON BLOCKS CONTAINING INFORMATION DEFINING THE CELL
  extern struct {
    struct complex_ zmult, wmap[mxwire];
    float x[mxwire],y[mxwire],v[mxwire],e[mxwire],d[mxwire],w[mxwire],
      u[mxwire],dens[mxwire],
      cosph2[mxwire],sinph2[mxwire],amp2[mxwire],b2sin[mxwire],coplan[4],
      vtplan[4],xmatt[5][mxmatt],ymatt[5][mxmatt],x3d[mx3d],y3d[mx3d],z3d[mx3d],
      e3d[mx3d],down[3],plstr1[3][mxpstr][5],plstr2[3][mxpstr][5],
      xmin,ymin,zmin,xmax,ymax,zmax,vmin,vmax,coplax,coplay,comatx,comaty,
      cotube,vttube,corvta,corvtb,
      corvtc,v0,sx,sy,sz,p1,p2,c1,kappa;
    
    int indsw[mxwire],nwire,nsw,ictype,mode,nxmatt,nymatt,ntube,mtube,
      n3d,ntermb,ntermp,ienbgf,
      indpla[5],npstr1[5],npstr2[5],
      indst1[mxpstr][5],indst2[mxpstr][5];
    
    int  ynplan[4],ynplax,ynplay,ynmatx,ynmaty,perx,pery,perz,polar,
      tube,permx,permy,permz,perax,peray,peraz,cnalso[mxwire],perrx,perry,perrz,lbgfmp,celset,ldipol;

  } celdat_;

  extern struct{
    char cellid[80];
    char wirtyp[mxwire];
    char platyp[5];
    char type[3];
    char pslab1[mxpstr][5];
    char pslab2[mxpstr][5];
  } celchr_;


  //FORTRAN SUBROUTINES REQUIRED TO ACCESS THE ELECTRIC FIELD
  void efield_(float* , float* , float* , float* , float* , float* ,
	       float* , float* , int* , int* );


  void setupanalyticfield_(int* ifail);

  void celchk_(int* ifail);
}



namespace Garfield {
  
  // ----------------------------------------------------------------
  class ComponentAnalyticField: public ComponentBase {
    
    RQ_OBJECT("ComponentAnalyticField")
 
      public:
    // Constructor
    ComponentAnalyticField(std::string cellName, float xl, float xm,float yl,float ym,float zl,float zm);  
    ComponentAnalyticField(std::string cellName);
    ComponentAnalyticField();
    
    // Destructor
    ~ComponentAnalyticField();
 

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Reqired methods
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
    
    void ElectricField(const double x, const double y, const double z,
                       double& ex, double& ey, double& ez, 
                       Medium*& m, int& status);
    // Calculate the drift field [V/cm] and potential [V] at (x, y, z)
    void ElectricField(const double x, const double y, const double z, 
		       double& ex, double& ey, double& ez, double& v, 
		       Medium*& m, int& status);
    // Calculate the voltage range [V]
    bool GetVoltageRange(double& vmin, double& vmax);
    
    // Magnetic field
    // Calculate the magnetic field [hGauss] at (x, y, z)
    void MagneticField(const double x, const double y, const double z,
		       double& bx, double& by, double& bz, int& status);
    // Reset the component
    void Reset();
     
     // Verify periodicities
    void UpdatePeriodicity();
    
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //  Other Virtual Methods
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
     void WeightingField(const double x, const double y, const double z,
                        double& wx, double& wy, double& wz,
                        const std::string label);

    double WeightingPotential(const double x, const double y, const double z,
                              const std::string label);

    void SetGeometry(GeometryBase* geo);


    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Methods unique to ComponentAnalyticField
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void Prepare();

    void CellInit();
    void CellType();
    
    void AddWire(float x, float y, float diameter, float voltage,float tension, float rho, 
		 float length, char label);
    void AddTube(float radius, float voltage, int numEdges, char label);
    void AddPlanes(bool plane1, float c1, float v1, char lab1,   
		   bool plane2, float c2, float v2, char lab2,
		   bool plane3, float c3, float v3, char lab3,
		   bool plane4, float c4, float v4, char lab4);

    //Rotates the cell. Originally the plane, wires and tubes are placed 
    //perpendicular to the xy-plane.
    //theta rotates y -> z 
    //phi  rotates z -> x
    //gamma rotates x -> y
    void SetRotation(double aTheta, double aPhi, double aGamma);
    //Calculated the rotation of a vector
    void Rotate(double x, double y, double z,
		double& xp, double& yp, double& zp,
		bool isInverse);
  
    //Sets the translation vector
    void SetTranslation(double xtrans, double ytrans, double ztrans);
    //Calculates the translation of a vector
    void Translate(double x, double y, double z,
		   double& xp, double& yp, double& zp,
		   bool isInverse);
  
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Methods for setting the periodicities
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
    //Setting periodicities
    void EnableXperiod(){celdat_.perx=true;}
    void DisableXperiod(){celdat_.perx=false;}
    void EnableYperiod(){celdat_.pery=true;}
    void DisableYperiod(){celdat_.pery=false;}
    void EnableZperiod(){celdat_.perz=true;}
    void DisableZperiod(){celdat_.perz=false;}
    
    //Setting mirror periodicity.
    void EnableMirrorXperiod(){celdat_.permx=true;}
    void DisableMirrorXperiod(){celdat_.permx=false;}
    void EnableMirrorYperiod(){celdat_.permy=true;}
    void DisableMirrorYperiod(){celdat_.permy=false;} 
    void EnableMirrorZperiod(){celdat_.permz=true;}
    void DisableMirrorZperiod(){celdat_.permz=false;}
    
    //Setting axial periodicity
    void EnableAxialXperiod(){celdat_.perax=true;}
    void DisableAxialXperiod(){celdat_.perax=false;}
    void EnableAxialYperiod(){celdat_.peray=true;}
    void DisableAxialYperiod(){celdat_.peray=false;}
    void EnableAxialZperiod(){celdat_.peraz=true;}
    void DisableAxialZperiod(){celdat_.peraz=false;}
    
    //Setting rotation symmetry
    void EnableRotXperiod(){celdat_.perrx=true;}
    void DisableRotXperiod(){celdat_.perrx=false;}
    void EnableRotYperiod(){celdat_.perry=true;}
    void DisableRotYperiod(){celdat_.perry=false;}
    void EnableRotZperiod(){celdat_.perrz=true;}
    void DisableRotZperiod(){celdat_.perrz=false;}

    private:
    bool cellok;
    bool isRotated;
    bool isTranslated;

    double theta, phi, gamma;
    double xTran, yTran, zTran;

    
    //These methods are used to translate incoming coordinates into local the
    //local coordinate frame and out going coordinates into the global coordinate
    //frame.

    void Global2Internal(double& xin, double& yin, double& zin, 
			 double xout, double yout, double zout);
    void Internal2Global(double xin, double yin, double zin, 
			 double& xout, double& yout, double& zout);

    void SetxTran(double value){xTran = value;}
    void SetyTran(double value){yTran = value;}
    void SetzTran(double value){zTran = value;}
  };
}

#endif
