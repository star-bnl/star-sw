#ifndef G_COMPONENT_ANALYTIC_FIELD_H
#define G_COMPONENT_ANALYTIC_FIELD_H

#include <string>

#include <RQ_OBJECT.h>

#include "ComponentBase.hh"
#include "GeometryBase.hh"

// Parameters in Fortran subroutines
static const int mxwire = 2000;
static const int mxmatt = 10;
static const int mx3d = 100;
static const int mxpstr = 100;

// Structure defining a complex number

struct complex_{
  float real;
  float imag;
};

extern "C" {
  // COMMON BLOCKS CONTAINING INFORMATION DEFINING THE CELL
  extern struct {
    struct complex_ zmult, wmap[mxwire];
    float x[mxwire], y[mxwire], v[mxwire], e[mxwire], d[mxwire], w[mxwire],
      u[mxwire], dens[mxwire],
      cosph2[mxwire], sinph2[mxwire], amp2[mxwire], b2sin[mxwire],
      coplan[4], vtplan[4], 
      xmatt[5][mxmatt], ymatt[5][mxmatt],
      x3d[mx3d], y3d[mx3d], z3d[mx3d], e3d[mx3d],
      down[3], plstr1[3][mxpstr][5], plstr2[3][mxpstr][5],
      xmin, ymin, zmin, xmax, ymax, zmax, vmin, vmax,
      coplax, coplay, comatx, comaty,
      cotube, vttube, corvta, corvtb, corvtc,
      v0, sx, sy, sz, p1, p2, c1, kappa;
    
    int indsw[mxwire], nwire, nsw, ictype, mode, nxmatt, nymatt, ntube, mtube,
      n3d, ntermb, ntermp, ienbgf,
      indpla[5], npstr1[5], npstr2[5],
      indst1[mxpstr][5], indst2[mxpstr][5];
    
    int ynplan[4], ynplax, ynplay, ynmatx, ynmaty, 
      perx, pery, perz, polar, tube, 
      permx, permy, permz, perax, peray, peraz, cnalso[mxwire],
      perrx, perry, perrz, lbgfmp, celset, ldipol;

  } celdat_;

  extern struct {
    char cellid[80];
    char wirtyp[mxwire];
    char platyp[5];
    char type[3];
    char pslab1[mxpstr][5];
    char pslab2[mxpstr][5];
  } celchr_;


  // Fortran subroutines required to access the electric field
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
    // Constructors
    ComponentAnalyticField(std::string cellName, 
                             float xl, float xm, float yl, float ym, 
                             float zl, float zm);
    ComponentAnalyticField(std::string cellName);
    ComponentAnalyticField();
    
    // Destructor
    ~ComponentAnalyticField() {}
 
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

    void AddWire(float x, float y, float diameter, float voltage, char label,
                 float tension = 50., float length = 100., float rho = 19.3);
    void AddTube(float radius, float voltage, int numEdges, char label);
    void AddPlanes(bool plane1, float c1, float v1, char lab1,   
		   bool plane2, float c2, float v2, char lab2,
		   bool plane3, float c3, float v3, char lab3,
		   bool plane4, float c4, float v4, char lab4);

    // Set the periodic length [cm] in x/y direction
    void SetPeriodicityX(const double s);
    void SetPeriodicityY(const double s);
       
    bool Prepare();

    //Rotates the cell. Originally the plane, wires and tubes are placed 
    //perpendicular to the xy-plane.
    //theta rotates y -> z 
    //phi  rotates z -> x
    //gamma rotates x -> y
    void SetRotation(double aTheta, double aPhi, double aGamma);
    // Sets the translation vector
    void SetTranslation(double xtrans, double ytrans, double ztrans);

  private:
    
    bool cellok;
    bool isRotated;
    bool isTranslated;

    // Periodic lengths
    double sx, sy;

    double cosTheta, sinTheta, cosPhi,sinPhi, cosGamma, sinGamma;
    double cosMinusTheta, sinMinusTheta, cosMinusPhi,sinMinusPhi, 
           cosMinusGamma, sinMinusGamma;
    double xTran, yTran, zTran;

    // Reset the component
    void Reset();     
     // Verify periodicities
    void UpdatePeriodicity();

    void CellInit();
    bool CellType();
    
    // These methods are used to translate incoming coordinates into the
    // local coordinate frame and out going coordinates into the 
    // global coordinate frame.

    void Translate(double& x, double& y, double& z,
		   const bool isInverse) const {

      if (isInverse) {
        x += xTran; y += yTran; z += zTran;
        return;
      } 
      x -= xTran; y -= yTran; z -= zTran;
        
    }
    void Rotate(double x, double y, double z,
                double& xp, double& yp, double& zp, bool isInverse);    

    void Global2Internal(double& xl, double& yl, double& zl, 
			 double xw, double yw, double zw) {
                         
      xl = xw; yl = yw; zl = zw;
      if (isTranslated) Translate(xl, yl, zl, false);
      if (isRotated) Rotate(xl, yl, zl, xl, yl, zl, false);
    
    } 
                         
    void Internal2Global(double xl, double yl, double zl, 
			 double& xw, double& yw, double& zw) {
      
      xw = xl; yw = yl; zw = zl;
      if (isRotated) Rotate(xw, yw, zw, xw, yw, zw, true);
      if (isTranslated) Translate(xw, yw, zw, true);
      
    }

};

}

#endif
