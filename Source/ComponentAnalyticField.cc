#include <iostream>
#include <string>
#include <math.h>
#include "ComponentAnalyticField.hh"

namespace Garfield {
  
//**************************************************************************/
//                               CONSTRUCTOR
//**************************************************************************/
ComponentAnalyticField::ComponentAnalyticField(std::string cellName, float xl, float xm,float yl,float ym,float zl,float zm) {

  CellInit();
  strcpy(celchr_.cellid, cellName.c_str());
  celdat_.xmin = xl;
  celdat_.xmax = xm;
  celdat_.ymin = yl;
  celdat_.ymax = ym;
  celdat_.zmin = zl;
  celdat_.zmax = zm;
  cellok = false;
  isRotated = false;
  xTran = yTran = zTran = 0.0;

}
  
ComponentAnalyticField::ComponentAnalyticField(std::string cellName) {

  CellInit();
  strcpy(celchr_.cellid, cellName.c_str());
  cellok = false;
  isRotated = false;
  xTran = yTran = zTran = 0.0;  

}
  
ComponentAnalyticField::ComponentAnalyticField() {

  CellInit();
  strcpy(celchr_.cellid, "");
  celdat_.xmin = 0.0;
  celdat_.xmax = 0.0;
  celdat_.ymin = 0.0;
  celdat_.ymax = 0.0;
  celdat_.zmin = 0.0;
  celdat_.zmax = 0.0;
  cellok = false;
  isRotated = false;
  xTran = yTran = zTran = 0.0;

}
  
  
//**************************************************************************/
//                               Initialize celdat_
//**************************************************************************/
void 
ComponentAnalyticField::CellInit() {

  // SETTING BOOLEANS
  for (int i = 0; i < 4; i++) celdat_.ynplan[i] = false;
  
  celdat_.perx = false; celdat_.pery = false; celdat_.perz = false;
  celdat_.ynplax = false; celdat_.ynplay = false;
  celdat_.ynmatx = false; celdat_.ynmaty = false;
  celdat_.polar = false;
  celdat_.tube = false;
  celdat_.permx = false; celdat_.permy = false; celdat_.permz = false;
  celdat_.perax = false; celdat_.peray = false; celdat_.peraz = false;
  celdat_.perrx = false; celdat_.perry = false; celdat_.perrz = false;
  
  for (int i = 0; i < mxwire; i++) celdat_.cnalso[i] = false;
  
  celdat_.lbgfmp = false;
  celdat_.celset = false;
  celdat_.ldipol = false;
  
  //SETTING INTEGERS
  for (int i = 0; i < mxwire; i++) celdat_.indsw[i]=0;
    
  for (int i = 0; i < mxpstr; i++) {
    for (int j = 0; j < 5; j++) {
      celdat_.indst1[i][j] = 0;
      celdat_.indst2[i][j] = 0;
    }
  }
    
  celdat_.nwire = 0;
  celdat_.nsw = 0;
  celdat_.ictype = 0;
  celdat_.mode = 0;
  celdat_.ntube = 0;
  celdat_.mtube = 0;
  celdat_.nxmatt = 0; celdat_.nymatt = 0; 
  celdat_.n3d = 0;
  celdat_.ntermb = 0;
  celdat_.ntermp = 0;
  celdat_.ienbgf = 0;
    
  for (int i = 0; i < 5; i++) {
    celdat_.indpla[i] = 0;
    celdat_.npstr1[i] = 0;
    celdat_.npstr2[i] = 0;
  }
    
  //SETTING FLOATS
  for (int i = 0; i < mxwire; i++) {
    celdat_.x[i] = 0.0; celdat_.y[i] = 0.0;
    celdat_.v[i] = 0.0; celdat_.e[i] = 0.0;
    celdat_.d[i] = 0.0; celdat_.w[i] = 0.0;
    celdat_.u[i] = 0.0;
    celdat_.dens[i] = 0.0;
    celdat_.cosph2[i] = 0.0; celdat_.sinph2[i] = 0.0;
    celdat_.amp2[i] = 0.0;
    celdat_.b2sin[i]=0.0;
  }
    
  for (int i = 0; i < 4; i++) {
    celdat_.coplan[i] = 0.0;
    celdat_.vtplan[i] = 0.0;
  }

  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < mxmatt; j++) {
      celdat_.xmatt[i][j] = 0.0; celdat_.ymatt[i][j] = 0.0;
    }
  }
    
  for (int i = 0; i < mx3d; i++) {
    celdat_.x3d[i] = 0.0; celdat_.y3d[i] = 0.0; celdat_.z3d[i] = 0.0;
    celdat_.e3d[i] = 0.0;
  }
    
  for (int i = 0; i < 3; i++) celdat_.down[i] = 0.0;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < mxpstr; j++) {
      for (int k = 0; k < 5; k++) {
        celdat_.plstr1[i][j][k] = 0.0;
        celdat_.plstr2[i][j][k] = 0.0;
      }
    }
  }
  celdat_.cotube = 0.0;
  celdat_.vttube = 0.0;
  celdat_.p1 = 0.0;
  celdat_.p2 = 0.0;
  celdat_.c1 = 0.0;
  celdat_.xmin = 0.0; celdat_.ymin = 0.0; celdat_.zmin = 0.0;
  celdat_.xmax = 0.0; celdat_.ymax = 0.0; celdat_.zmax = 0.0;
  celdat_.vmin = 0.0; celdat_.vmax = 0.0;
  celdat_.coplax = 0.0; celdat_.coplay = 0.0;
  celdat_.comatx = 0.0; celdat_.comaty = 0.0;
  celdat_.corvta = 0.0; celdat_.corvtb = 0.0; celdat_.corvtc = 0.0;
  celdat_.v0 = 0.0;
  celdat_.sx = 0.0; celdat_.sy = 0.0; celdat_.sz = 0.0;    
  celdat_.kappa = 0.0;
    
  //SETTING CHARS IN CELCHR_
  for (int i = 0; i < 80; i++) celchr_.cellid[i] = ' ';
  for (int i = 0; i < 3; i++) celchr_.type[i] = ' ';
  for (int i = 0; i < mxwire; i++) celchr_.wirtyp[i] = ' ';
  for (int i = 0; i < 5; i++) celchr_.platyp[i] = ' ';
  for (int i = 0; i < mxpstr; i++) {
    for(int j = 0;j < 5;j++) {
      celchr_.pslab1[i][j] = ' ';
      celchr_.pslab2[i][j] = ' ';
    }
  }
    
  std::cout << "ComponentAnalyticField:CellInit:\n";
  std::cout << "    Cell created and initialized.\n";

}
  
//**************************************************************************/
//                               Add a wire
//**************************************************************************/
void 
ComponentAnalyticField::AddWire(float x, float y, float diameter, 
                                float voltage,float tension, float rho, 
                                float length, char label) {
    
  bool isOkay = true;
  // Bounding points in global coordinates
  double xmin, ymin, zmin, xmax, ymax, zmax;
  // Locations of wire end points in local coordinates
  double e1xl, e1yl, e1zl, e2xl, e2yl, e2zl;
  // Locations of wire end points in global coordinates
  double e1xw, e1yw, e1zw, e2xw, e2yw, e2zw;
    
  if (theGeometry == 0) {
    std::cerr << "ComponentAnalyticField::AddWire:\n";
    std::cerr << "    Geometry not yet set.\n";
    std::cerr << "    Unable to place wire at ("<< x <<", "<<y <<") .\n";
    return;
  }

  theGeometry->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax);
    
  e1xl = double(x);
  e1yl = double(y);
  e1zl = double(length / 2.0);
  
  e2xl = double(x);
  e2yl = double(y);
  e2zl = double(-length / 2.0);
  
  // Transform local coordinates into global
  Internal2Global(e1xl, e1yl, e1zl, e1xw, e1yw, e1zw);
  Internal2Global(e2xl, e2yl, e2zl, e2xw, e2yw, e2zw);
    
  /*std::cerr <<"End point 1 in local:\n";
  std::cerr <<e1xl << "\t" << e1yl << "\t" << e1zl <<"\n";
  std::cerr <<"End point 1 in global:\n";
  std::cerr <<e1xw << "\t" << e1yw << "\t" << e1zw <<"\n";*/
      
  const double e1x = e1xw;
  const double e1y = e1yw; 
  const double e1z = e1zw;
  const double e2x = e2xw; 
  const double e2y = e2yw;
  const double e2z = e2zw;

  // Now check to see if these points are contained in the geometry
  if (!theGeometry->IsInside(e1x, e1y, e1z)) {
    isOkay = false;
    std::cerr << "Unable to place wire at (" << x << ", " << y 
              << ") local frame.\n";
    std::cerr << "End point (" << e1x << ", " << e1y << ", " << e1z 
              << ") outside.\n";
    std::cerr << "Boundaries are:\n";
    std::cerr << "x: " << xmin << "\t" << xmax << "\n";
    std::cerr << "y: " << ymin << "\t" << ymax << "\n";
    std::cerr << "z: " << zmin << "\t" << zmax << "\n";
  }
  if (!theGeometry->IsInside(e2x, e2y, e2z)) {
    isOkay = false;
    std::cerr << "Unable to place wire at (" << x << ", " << y 
              << ") local frame.\n";
    std::cerr << "End point (" << e2x << ", " << e2y << ", " << e2z 
              << ") outside.\n";
    std::cerr << "Boundaries are:\n";
    std::cerr << "x: " << xmin << "\t" << xmax << "\n";
    std::cerr << "y: " << ymin << "\t" << ymax << "\n";
    std::cerr << "z: " << zmin << "\t" << zmax << "\n";
  }
   
  // if (!isOkay) return;
    
  celdat_.x[celdat_.nwire] = x;
  celdat_.y[celdat_.nwire] = y;
  if (diameter <= 0.0) {
    std::cerr << "Unphysical wire diameter.\n";
  } else {
    celdat_.d[celdat_.nwire] = diameter;
  }
      
  celdat_.v[celdat_.nwire] = voltage;
  if (tension <= 0.0) {
    std::cerr << "Unphysical wire tension.\n";
  } else {
    celdat_.w[celdat_.nwire] = tension;
  }
      
  if (rho <= 0.0) {
    std::cerr << "Unphysical wire density.\n";
  } else {
    celdat_.dens[celdat_.nwire] = rho;
  }

  if (length <= 0.0) {
    std::cerr << "Unphysical wire length.\n";
  } else {
    celdat_.u[celdat_.nwire] = length;
  }
      
  celchr_.wirtyp[celdat_.nwire] = label;
  if (celdat_.nwire + 1 > mxwire){
    std::cerr << "Maximum number of wires reached.\n";
  } else {
    celdat_.nwire = celdat_.nwire + 1;  
  }

}

//**************************************************************************/
//                  Add tube
//**************************************************************************/
void 
ComponentAnalyticField::AddTube(float radius, float voltage, int numEdges, 
                                char label) {

  bool isOkay = true;
  // Boundaries of solid
  double xmin, ymin, zmin, xmax, ymax, zmax;
  // Four points on wall of tube
  // Locations of wall on axis in local coordinates
  double p1xl, p2xl, p3yl, p4yl;
  // Locations of wall on axis in global coordinates
  double p1xw, p1yw, p1zw;
  double p2xw, p2yw, p2zw;
  double p3xw, p3yw, p3zw;
  double p4xw, p4yw, p4zw;

  p1xl =  radius;
  p2xl = -radius;
  p3yl =  radius;
  p4yl = -radius;

  if (theGeometry == 0) {    
    std::cerr << "Geometry not yet set for ComponentAnalyticField.\n";
    std::cerr << "Unable to place tube.\n";
    return;
  }

  theGeometry->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax);

  // Transform local coordinates into global
  Internal2Global(p1xl, 0.0, 0.0, p1xw, p1yw, p1zw);
  Internal2Global(p2xl, 0.0, 0.0, p2xw, p2yw, p2zw);  
  Internal2Global(0.0, p3yl, 0.0, p3xw, p3yw, p3zw);
  Internal2Global(0.0, p4yl, 0.0, p4xw, p4yw, p4zw);

  if (fabs(p1xw) > xmax || fabs(p1yw) > ymax || fabs(p1zw) > zmax) {
    std::cerr << "Tube point 1 is out of bounds.\n";
    std::cerr << "(" << p1xw << ", "<< p1yw << ", " << p1zw << ")\n";
  }
  if (fabs(p2xw) > xmax || fabs(p2yw) > ymax || fabs(p2zw) > zmax) {
    std::cerr << "Tube point 2 is out of bounds.\n";
    std::cerr << "(" << p2xw << ", " << p2yw << ", " << p2zw << ")\n";
  }
  if (fabs(p3xw) > xmax || fabs(p3yw) > ymax || fabs(p3zw) > zmax) {
    std::cerr << "Tube point 3 is out of bounds.\n";
    std::cerr << "(" << p3xw << ", " << p3yw << ", " << p3zw << ")\n";
  }
  if (fabs(p4xw) > xmax || fabs(p4yw) > ymax || fabs(p4zw) > zmax) {
    std::cerr << "Tube point 4 is out of bounds.\n";
    std::cerr << "(" << p4xw << ", " << p4yw << ", " << p4zw << ")\n"; 
  }

  // Setting Garfield Coordinate system flags
  celdat_.tube = true;
  celdat_.polar = false;
    
  if (radius <= 0.0) {
    std::cerr << "Unphysical tube radius.\n";
  } else {
    celdat_.cotube = radius;
  }
  celdat_.vttube = voltage;
    
  if (numEdges < 3 && numEdges > 0) {
    std::cerr << "Unphysical number of tube edges (cant be 1 or 2).\n";
  } else {
    celdat_.ntube = numEdges;
  }
    
  celchr_.platyp[4] = label;
  celdat_.indpla[4] = -1;
  celdat_.npstr1[4] = -1;
  celdat_.npstr2[4] = -1;

}

//**************************************************************************/
//                               Add planes
//**************************************************************************/
void 
ComponentAnalyticField::AddPlanes(bool plane1, float c1, float v1, char lab1,  
                                  bool plane2, float c2, float v2, char lab2,
                                  bool plane3, float c3, float v3, char lab3,
                                  bool plane4, float c4, float v4, char lab4) {

  bool isOkay = true;
  // Bounding points in global coordinates
  double xmin, ymin, zmin, xmax, ymax, zmax;
  // Locations of plane on axis in local coordinates
  double p1xl, p2xl, p3yl, p4yl;
  // Locations of plane on axis in global coordinates
  double p1xw = 0., p1yw = 0., p1zw = 0.;
  double p2xw = 0., p2yw = 0., p2zw = 0.;
  double p3xw = 0., p3yw = 0., p3zw = 0.;
  double p4xw = 0., p4yw = 0., p4zw = 0.;
 
  if (theGeometry == 0) {
    std::cerr <<"Geometry not yet set for ComponentAnalyticField.\n";
    std::cerr <<"Unable to place planes.\n";
    isOkay = false; 
    return;
  }

  theGeometry->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax);
  
  p1xl = double(c1);
  p2xl = double(c2);
  p3yl = double(c3);
  p4yl = double(c4);
  
  // Transform local coordinates into global
  Internal2Global(p1xl, 0.0, 0.0, p1xw, p1yw, p1zw);
  Internal2Global(p2xl, 0.0, 0.0, p2xw, p2yw, p2zw);  
  Internal2Global(0.0, p3yl, 0.0, p3xw, p3yw, p3zw);
  Internal2Global(0.0, p4yl, 0.0, p4xw, p4yw, p4zw);
  
  // Now check to see if these points are contained in the geometry
  // Cant use theGeometry->IsInside because it doesnt allow for the planes 
  // to be on the walls of the container.
  if (fabs(p1xw) > xmax || fabs(p1yw) > ymax || fabs(p1zw) > zmax) {
    std::cerr << "Plane 1 is out of bounds.\n";
    std::cerr << "(" << p1xw << ", " << p1yw << ", " << p1zw << ")\n";
    std::cerr << "Bounds are:\n";
    std::cerr << xmin << "\t" << xmax << "\n";
  }
  if (fabs(p2xw) > xmax || fabs(p2yw) > ymax || fabs(p2zw) > zmax) {
    std::cerr << "Plane 2 is out of bounds.\n";
    std::cerr << "(" << p2xw << ", " << p2yw << ", " << p2zw << ")\n";
    std::cerr << "Bounds are:\n";
    std::cerr << xmin << "\t" << xmax << "\n";
  }
  if (fabs(p3xw) > xmax || fabs(p3yw) > ymax || fabs(p3zw) > zmax) {
    std::cerr << "Plane 3 is out of bounds.\n";
    std::cerr << "(" << p3xw << ", " << p3yw << ", " << p3zw << ")\n";
    std::cerr << "Bounds are:\n";
    std::cerr << ymin << "\t" << ymax << "\n";
  }
  if(fabs(p4xw) > xmax || fabs(p4yw) > ymax || fabs(p4zw) > zmax) {
    std::cerr << "Plane 4 is out of bounds.\n";
    std::cerr << "(" << p4xw << ", " << p4yw << ", " << p4zw << ")\n"; 
    std::cerr << "Bounds are:\n";
    std::cerr << ymin << "\t" << ymax << "\n";
  }
  
  // SETUP FOR PLANE 1
  celdat_.ynplan[0] = plane1;
  celdat_.coplan[0] = c1;
  celdat_.vtplan[0] = v1;
  celchr_.platyp[0] = lab1;
  
  // SETUP FOR PLANE 2
  celdat_.ynplan[1] = plane2;
  celdat_.coplan[1] = c2;
  celdat_.vtplan[1] = v2;
  celchr_.platyp[1] = lab2;
  
  // SETUP FOR PLANE 3
  celdat_.ynplan[2] = plane3;
  celdat_.coplan[2] = c3;
  celdat_.vtplan[2] = v3;
  celchr_.platyp[2] = lab3;
  
  // SETUP FOR PLANE 4
  celdat_.ynplan[3] = plane4;
  celdat_.coplan[3] = c4;
  celdat_.vtplan[3] = v4;
  celchr_.platyp[3] = lab4;

}

//**************************************************************************/
//                     Check the Cell type
//                     this is used in E-field calculations 
//**************************************************************************/

void
ComponentAnalyticField::CellType(){

  // DEAL WITH TUBE GEOMETRIES
  if (celdat_.tube) {
    if (celdat_.ntube == 0) {
      if (celdat_.pery) {
        strcpy(celchr_.type, "D2 ");
      } else {
        strcpy(celchr_.type, "D1 ");
      }
    } else if (celdat_.ntube >= 3 && celdat_.ntube <= 8) {
      if (celdat_.pery) {
        strcpy(celchr_.type, "D4 ");
      } else {
        strcpy(celchr_.type, "D3 ");
      }
    } else {
      std::cout <<"!!!!!! CELTYP WARNING : Potentials not yet available, using a round tube.\n";
      strcpy(celchr_.type, "D3 ");
      celdat_.ntube = 0;
    }
  } else {
    // FIND 'A' TYPE cell
    if (!(celdat_.perx || celdat_.pery) && 
        !(celdat_.ynplan[0] && celdat_.ynplan[1]) && 
        !(celdat_.ynplan[2] && celdat_.ynplan[3])) {
      strcpy(celchr_.type, "A  ");
    }

    //FIND 'B1X' type cell
    if (celdat_.perx && !celdat_.pery && 
        !(celdat_.ynplan[0] || celdat_.ynplan[1]) && 
        !(celdat_.ynplan[2] && celdat_.ynplan[3])) {
      strcpy(celchr_.type, "B1X");
    }

    //FIND 'B1Y' TYPE CELLS
    if (celdat_.pery && !celdat_.perx && 
        !(celdat_.ynplan[0] && celdat_.ynplan[1]) && 
        !(celdat_.ynplan[2] || celdat_.ynplan[3])) {
      strcpy(celchr_.type, "B1Y");
    }

    // Find the 'B2X' type cell.
    if (celdat_.perx && !celdat_.pery && 
        !(celdat_.ynplan[2] && celdat_.ynplan[3])) {
      strcpy(celchr_.type, "B2X");
    }

    if (!(celdat_.perx || celdat_.pery) && 
        !(celdat_.ynplan[2] && celdat_.ynplan[3]) &&
         (celdat_.ynplan[0] && celdat_.ynplan[1])) {
      celdat_.sx = fabs(celdat_.coplan[1] - celdat_.coplan[0]);      
      strcpy(celchr_.type, "B2X");
    }

    // Find the 'B2Y' type cell.
    if (celdat_.pery && !celdat_.perx && 
        !(celdat_.ynplan[0] && celdat_.ynplan[1])) {
      strcpy(celchr_.type, "B2Y");
    }
	
    if (!(celdat_.perx || celdat_.pery) &&
        !(celdat_.ynplan[0] && celdat_.ynplan[1]) && 
         (celdat_.ynplan[2] && celdat_.ynplan[3])) {
      celdat_.sy = fabs(celdat_.coplan[3] - celdat_.coplan[2]);
      strcpy(celchr_.type, "B2Y");  
    }

    // Find the 'C1 ' type cell.
    if (!(celdat_.ynplan[0] || celdat_.ynplan[1] || 
          celdat_.ynplan[2] || celdat_.ynplan[3]) && 
        celdat_.perx && celdat_.pery) {
      strcpy(celchr_.type, "C1 ");
    }

    //Find the 'C2X' type cell.
    if (!((celdat_.ynplan[2] && celdat_.pery) || 
          (celdat_.ynplan[2] && celdat_.ynplan[3]))) {
      if (celdat_.ynplan[0] && celdat_.ynplan[1]) {
        celdat_.sx = fabs(celdat_.coplan[1] - celdat_.coplan[0]);
        strcpy(celchr_.type, "C2X");
      }
      if (celdat_.perx && celdat_.ynplan[0]) {
        strcpy(celchr_.type, "C2X");
      }
    }

    // Find the 'C2Y' type cell.
    if (!((celdat_.ynplan[0] && celdat_.perx) || 
          (celdat_.ynplan[0] && celdat_.ynplan[1]))) {
      if (celdat_.ynplan[2] && celdat_.ynplan[3]) {
        celdat_.sy= fabs(celdat_.coplan[3] - celdat_.coplan[2]);
        strcpy(celchr_.type, "C2Y");
      }
      if (celdat_.pery && celdat_.ynplan[2]) {
        strcpy(celchr_.type, "C2Y");
      }
    }
    // Find the 'C3 ' type cell.
    if (celdat_.perx && celdat_.pery) {
      strcpy(celchr_.type, "C3 ");
    }
    if (celdat_.perx) {
      strcpy(celchr_.type, "C3 ");
      celdat_.sy = fabs(celdat_.coplan[3] - celdat_.coplan[2]);
    }
    if (celdat_.pery){
      celdat_.sx = fabs(celdat_.coplan[1] - celdat_.coplan[0]);
      strcpy(celchr_.type, "C3 ");
    }
    if (celdat_.ynplan[0] && celdat_.ynplan[1] && 
        celdat_.ynplan[2] && celdat_.ynplan[3]) {
      strcpy(celchr_.type, "C3 "); 
      celdat_.sx = fabs(celdat_.coplan[1] - celdat_.coplan[0]);
      celdat_.sy = fabs(celdat_.coplan[3] - celdat_.coplan[2]);
    }
  }

  celdat_.sx = fabs(celdat_.sx);
  celdat_.sy = fabs(celdat_.sy);

  // Store a numerical code for the cell type for greater efficiency.
  if (strcmp(celchr_.type, "A  ") == 0) {
    celdat_.ictype = 1;
  } else if (strcmp(celchr_.type, "B1X") == 0) {
    celdat_.ictype = 2;
  } else if (strcmp(celchr_.type, "B1Y") == 0) {
    celdat_.ictype = 3;
  } else if (strcmp(celchr_.type, "B2X") == 0) {
    celdat_.ictype = 4;
  } else if (strcmp(celchr_.type, "B2Y") == 0) {
    celdat_.ictype = 5;
  } else if (strcmp(celchr_.type, "C1 ") == 0) {
    celdat_.ictype = 6;
  } else if (strcmp(celchr_.type, "C2X") == 0) {
    celdat_.ictype = 7;
  } else if (strcmp(celchr_.type, "C2Y") == 0) {
    celdat_.ictype = 8;
  } else if (strcmp(celchr_.type, "C3 ") == 0) {
    celdat_.ictype = 9;
  } else if (strcmp(celchr_.type, "D1 ") == 0) {
    celdat_.ictype = 10;
  } else if (strcmp(celchr_.type, "D2 ") == 0) {
    celdat_.ictype = 11;
  } else if (strcmp(celchr_.type, "D3 ") == 0) {
    celdat_.ictype = 12;
  } else if (strcmp(celchr_.type, "D4 ") == 0) {
     celdat_.ictype = 13;
  } else {
    std::cerr << "ComponentAnalyticField::CellType:\n";
    std::cerr << "    Unknown cell type.\n";
    return;
  }

  std::cout << "ComponentAnalyticField::CellType:\n";
  std::cout << "    Cell is of type " << celchr_.type << "\n";

}

//**************************************************************************/
//               DETERMINES THE EFIELD AT A LOCATION     
//**************************************************************************/
/*       SUBROUTINE EFIELD(XIN,YIN,ZIN,EX,EY,EZ,ETOT,VOLT,IOPT,ILOC)
*-----------------------------------------------------------------------
*   EFIELD - Subroutine calculating the electric field and the potential
*            at a given place. It makes use of the routines POT...,
*            depending on the type of the cell.
*   VARIABLES : XPOS       : x-coordinate of the place where the field
*                            is to be calculated.
*               YPOS, ZPOS : y- and z-coordinates
*               EX, EY, EZ : x-, y-, z-component of the electric field.
*               VOLT       : potential at (XPOS,YPOS).
*               IOPT       : 1 if both E and V are required, 0 if only E
*                            is to be computed.
*               ILOC       : Tells where the point is located (0: normal
*                            I > 0: in wire I, -1: outside a plane,
*                            -5: in a material, -6: outside the mesh, 
*                            -10: unknown potential).
*   (Last changed on 28/ 9/07.)
*-----------------------------------------------------------------------
*/
void 
ComponentAnalyticField::ElectricField (
                              const double x, const double y, const double z,
                              double& ex, double& ey, double& ez, 
                              Medium*& m, int& status) {

  int opt = 0;
  int state;
  // FORTRAN CODE REQUIRES FLOATS
  float xpos, ypos, zpos;
  float exf, eyf, ezf;
  float etot, volt;
 
  // Global (or world) coordinates
  double xw = x, yw = y, zw = z;
  // Internal (or local) coordinates
  double xl,yl,zl;
     
  Global2Internal(xl, yl, zl, xw, yw, zw);

  xpos = float(xl);
  ypos = float(yl);
  zpos = float(zl);

  efield_(&xpos, &ypos, &zpos, &exf, &eyf, &ezf, &etot, &volt, &opt, &state);

  /*
  if (isRotated) {
    if (theta != 0.0) {
      exf = exf;
      eyf = eyf * cos(theta) - ezf * sin(theta);
      ezf = eyf * sin(theta) + ezf * cos(theta);
    }
    if (phi != 0.0) {
      exf =  exf * cos(phi) + ezf * sin(phi);
      eyf =  eyf;
      ezf = -exf * sin(phi) + ezf * cos(phi);
    }
    if(gamma != 0.0){
      exf = exf * cos(gamma) - eyf * sin(gamma);
      eyf = exf * sin(gamma) + eyf * cos(gamma);
      ezf = ezf;
    }

  }
  //*/

  if (isRotated) Rotate(ex, ey, ez, ex, ey, ez, false);

  ex = double(exf); 
  ey = double(eyf);
  ez = double(ezf);

  status = state;

  GetMedium(x, y, z, m);

}

// Calculate the drift field [V/cm] and potential [V] at (x, y, z)
void ComponentAnalyticField::ElectricField (
                               const double x, const double y, const double z, 
                               double& ex, double& ey, double& ez, double& v, 
                               Medium*& m, int& status) {
  int opt = 1;
  int state;
  // FORTRAN CODE REQUIRES FLOATS
  float xpos, ypos, zpos;
  float exf, eyf, ezf;
  float etot, vf;

  // Global (or world) coordinates
  double xw = x, yw = y, zw = z;

  // Internal (or local) coordinates
  double xl = 0., yl = 0., zl = 0.;
    
  Global2Internal(xl, yl, zl, xw, yw, zw);
   
  xpos = float(xl);
  ypos = float(yl);
  zpos = float(zl);

  efield_(&xpos, &ypos, &zpos, &exf, &eyf, &ezf, &etot, &vf, &opt, &state);

  if (isRotated) Rotate(ex, ey, ez, ex, ey, ez, false);

  ex = double(exf); 
  ey = double(eyf);
  ez = double(ezf);
  v = double(vf);
  status = state;

  GetMedium(x, y, z, m);
   
}

// Calculate the voltage range [V]
bool ComponentAnalyticField::GetVoltageRange(double& vmin, double& vmax) {

  if (cellok) {
    vmin = double(celdat_.vmin); 
    vmax = double(celdat_.vmax);
    return true;
  }
  
  std::cerr << "ComponentAnalyticField::GetVoltageRange:\n";
  std::cerr <<"    Unable to return voltage range since cell not setup yet.\n";
  return false;

}

// Reset the component
void ComponentAnalyticField::Reset() {}


void ComponentAnalyticField::Prepare() {

  int ifail = 0;

  celchk_(&ifail);
  if (ifail == 0){
    std::cout << "ComponentAnalyticField::Prepare:\n";
    std::cout << "    Cell check ok.\n";
    cellok = true;
  } else {
    std::cerr << "ComponentAnalyticField:Prepare:\n";
    std::cerr << "    Error while checking field.\n";
    cellok = false;
    return;
  }

  CellType();
  ifail = 0;

  setupanalyticfield_(&ifail);
  if (ifail == 0) {
    std::cout << "ComponentAnalyticField::Prepare:\n";
    std::cout << "    Field setup completed.\n";
    cellok = true;
  } else {
    std::cerr << "ComponentAnalyticField::Prepare:\n";
    std::cerr << "    Error while setting up field.\n";
    cellok = false;
  }

}
  
void 
ComponentAnalyticField::WeightingField(
                              const double x, const double y, const double z,
                              double& wx, double& wy, double& wz,
                              const std::string label) {
 
  std::cerr << "Method ComponentAnalyticField::WeightingField not yet implemented.\n";
   
}
  
double 
ComponentAnalyticField::WeightingPotential(
                             const double x, const double y, const double z,
                             const std::string label) {
  std::cerr << "Method ComponentAnalyticField::WeightingPotential not yet implemented.\n";
  return 0.0;

}

// Rotates the cell. Originally the plane, wires and tubes are placed 
// perpendicular to the xy-plane.
// theta rotates y -> z 
// phi  rotates z -> x
// gamma rotates x -> y

void 
ComponentAnalyticField::SetRotation(double aTheta, double aPhi, double aGamma) {

  cosPhi = cos(aPhi);
  sinPhi = sin(aPhi);
  cosGamma = cos(aGamma); 
  sinGamma = sin(aGamma);
  cosTheta = cos(aTheta);
  sinTheta = sin(aTheta);

  cosMinusPhi = cos(-1.*aPhi);
  sinMinusPhi = sin(-1.*aPhi);
  cosMinusGamma = cos(-1.*aGamma); 
  sinMinusGamma = sin(-1.*aGamma);
  cosMinusTheta = cos(-1.*aTheta);
  sinMinusTheta = sin(-1.*aTheta);

  if (aPhi == 0.0 && aGamma == 0.0 && aTheta == 0.0) {
    isRotated = false;
  } else {
    isRotated = true;
  }

}

// Calculate the rotation of a vector
void 
ComponentAnalyticField::Rotate(double x, double y, double z,
                               double& xp, double& yp, double& zp,
                               bool isInverse) {
  // Rotation matrices
  double Rx[3][3], Ry[3][3], Rz[3][3];
  // Rotated and nonrotated vectors
  double vp[3], v[3];
    
  vp[0] = vp[1] = vp[2] = 0.0;
  v[0] = x;
  v[1] = y;
  v[2] = z;
    
  // Make all matrices identity
    for(int i = 0; i < 3; i++){
      for(int j = 0; j < 3; j++){
	if(j != i){
	  Rx[i][j] = 0.0;
	  Ry[i][j] = 0.0;
	  Rz[i][j] = 0.0;
	}
	else{
	  Rx[i][j] = 1.0;
	  Ry[i][j] = 1.0;
	  Rz[i][j] = 1.0;
	}
      }
    }
    
    //Fill the rotation matrices
    if(!isInverse){
      Rx[1][1] = cosTheta;
      Rx[2][2] = cosTheta;
      Rx[1][2] = -sinTheta;
      Rx[2][1] = sinTheta;
      
      Ry[0][0] = cosPhi;
      Ry[0][2] = sinPhi;
      Ry[2][0] = -sinPhi;
      Ry[2][2] = cosPhi;
      
      Rz[0][0] = cosGamma;
      Rz[0][1] = -sinGamma;
      Rz[1][0] = sinGamma;
      Rz[1][1] = cosGamma;
    }
    else{
      Rx[1][1] = cosMinusTheta;
      Rx[2][2] = cosMinusTheta;
      Rx[1][2] = -sinMinusTheta;
      Rx[2][1] = sinMinusTheta;
      
      Ry[0][0] = cosMinusPhi;
      Ry[0][2] = sinMinusPhi;
      Ry[2][0] = -sinMinusPhi;
      Ry[2][2] = cosMinusPhi;
      
      Rz[0][0] = cosMinusGamma;
      Rz[0][1] = -sinMinusGamma;
      Rz[1][0] = sinMinusGamma;
      Rz[1][1] = cosMinusGamma;
    }

    //Do mx multiplication
    if(!isInverse){
      for(int i = 0; i < 3; i++){
	for(int j = 0; j < 3; j++){
	  vp[i] += v[j]*Rx[i][j];
	}
      }
      
      v[0] = v[1] = v[2] = 0.0;
      for(int i = 0; i < 3; i++){
	for(int j = 0; j < 3; j++){
	  v[i] += vp[j]*Ry[i][j];
	}
      }
      
      vp[0] = vp[1] = vp[2] = 0.0;
      for(int i = 0; i < 3; i++){
	for(int j = 0; j < 3; j++){
	  vp[i] += v[j]*Rz[i][j];
	}
      }
    }
    else{
      for(int i = 0; i < 3; i++){
	for(int j = 0; j < 3; j++){
	  vp[i] += v[j]*Rz[i][j];
	}
      }
      
      v[0] = v[1] = v[2] = 0.0;
      for(int i = 0; i < 3; i++){
	for(int j = 0; j < 3; j++){
	  v[i] += vp[j]*Ry[i][j];
	}
      }
      
      vp[0] = vp[1] = vp[2] = 0.0;
      for(int i = 0; i < 3; i++){
	for(int j = 0; j < 3; j++){
	  vp[i] += v[j]*Rx[i][j];
	}
      }
    }

    xp = vp[0];
    yp = vp[1];
    zp = vp[2];

  }
  
  
// Sets the translation vector
void 
ComponentAnalyticField::SetTranslation(double xtrans, double ytrans, double ztrans) {

  xTran = xtrans;
  yTran = ytrans;
  zTran = ztrans;
    
  if (xTran == 0.0 && yTran == 0.0 && zTran == 0.0) {
    isTranslated = false;
  } else {
    isTranslated = true;
  }

}
  
// Calculates the translation of a vector
void 
ComponentAnalyticField::Translate(double x, double y, double z,
                                  double& xp, double& yp, double& zp,
                                  bool isInverse) {
  if (isInverse) {
    xTran = -xTran;
    yTran = -yTran;
    zTran = -zTran;
  }
    
  xp = xp - xTran;
  yp = yp - yTran;
  zp = zp - zTran;

}

// These methods are used to translate incoming coordinates into local the
// local coordinate frame and out going coordinates into the global coordinate
// frame.
  
void 
ComponentAnalyticField::Global2Internal(double& xl, double& yl, double& zl, 
                                        double xw, double yw, double zw) {

  if (isRotated || isTranslated) {
    Translate(xl, yl, zl, xl, yl, zl, false);
    Rotate(xw, yw, zw, xl, yl, zl, false);
  } else {
    xl = xw;
    yl = yw;
    zl = zw;
  }

}

void 
ComponentAnalyticField::Internal2Global(double xl, double yl, double zl, 
                                        double& xw, double& yw, double& zw) {

  if (isRotated || isTranslated) {
    Rotate(xl, yl, zl, xw, yw, zw, true);
    Translate(xw, yw, zw, xw, yw, zw, true);
  } else {
    xw = xl;
    yw = yl;
    zw = zl;
  }

}

// This method takes the same form as the SetGeometry method in ComponentBase,
// however it also takes the position of the geometry and calculates the 
// translation of the "local" coordinate system used for field calculations.
void 
ComponentAnalyticField::SetGeometry(GeometryBase* geo) {
 
  // Make sure the geometry is defined
  if (geo == 0) {
    std::cerr << "ComponentBase::SetGeometry:" << std::endl;
    std::cerr << "    Geometry pointer is null." << std::endl;
    return;
  }
   
  theGeometry = geo;

  // Variables used in determining the center of the geometry
  double xmin, ymin, zmin, xmax, ymax, zmax;

  if (!theGeometry->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << "ComponentAnalyticField::SetGeometry:\n";
    std::cerr << "    Could not set translation.\n";
    std::cerr << "    No solids attached to geometry.";
  } else {
    SetxTran( (xmin+xmax) / 2.0);
    SetyTran( (ymin+ymax) / 2.0);
    SetzTran( (zmin+zmax) / 2.0);
  }

  if (fabs(xTran) > 1.e-8 || fabs(yTran) > 1.e-8 || fabs(zTran) > 1.e-8) {
    isTranslated = true;
  }

}

}
