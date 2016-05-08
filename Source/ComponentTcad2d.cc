#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <algorithm>
#include <cmath>

#include "ComponentTcad2d.hh"

namespace Garfield {

ComponentTcad2d::ComponentTcad2d()
    : ComponentBase(),
      nRegions(0),
      nVertices(0),
      nElements(0),
      hasPotential(false),
      hasField(false),
      hasElectronMobility(false),
      hasHoleMobility(false),
      nDonor(0),
      nAcceptor(0),
      pMin(0.),
      pMax(0.),
      hasRangeZ(false),
      lastElement(0) {

  m_className = "ComponentTcad2d";

  regions.reserve(10);
  regions.clear();
  vertices.reserve(1000);
  vertices.clear();
  elements.reserve(1000);
  elements.clear();

  donorElectronXsec.clear();
  donorHoleXsec.clear();
  acceptorElectronXsec.clear();
  acceptorHoleXsec.clear();
  donorConc.clear();
  acceptorConc.clear();

  for (int i = nMaxVertices; i--;) w[i] = 0.;
}


bool ComponentTcad2d::SetDonorXsec(const int donorNumber, const double eXsec, const double hXsec){
  if (nDonor < 1) {
    std::cerr << m_className << "::SetDonorXsec:\n";
    std::cerr << "    No donor occupation maps exists.\n";
    return false;
  }
  if (donorNumber > nDonor - 1) {
    std::cerr << m_className << "::SetDonorXsec:\n";
    std::cerr << "    This donor occupation map does not exist.\n";
    return false;
  }
  if (donorNumber < 0) {
    std::cerr << m_className << "::SetDonorXsec:\n";
    std::cerr << "    Negative indicies not allowed.\n";
    return false;
  }
  donorElectronXsec[donorNumber] = eXsec; 
  donorHoleXsec[donorNumber] = hXsec; 

  validXsec = true;
  for(int i = 0; i<nDonor; i++){
    if(donorElectronXsec[i] < 0) validXsec = false;
    if(donorHoleXsec[i] < 0) validXsec = false;
  }
  for(int i = 0; i<nAcceptor; i++){
    if(acceptorElectronXsec[i] < 0) validXsec = false;
    if(acceptorHoleXsec[i] < 0) validXsec = false;
  }

  return true;
}

bool ComponentTcad2d::SetAcceptorXsec(const int acceptorNumber, const double eXsec, const double hXsec){
  if (nAcceptor < 1) {
    std::cerr << m_className << "::SetAcceptorXsec:\n";
    std::cerr << "    No acceptor occupation maps exists.\n";
    return false;
  }
  if (acceptorNumber > nAcceptor - 1) {
    std::cerr << m_className << "::SetAcceptorXsec:\n";
    std::cerr << "    This acceptor occupation map does not exist.\n";
    return false;
  }
  if (acceptorNumber < 0) {
    std::cerr << m_className << "::SetAcceptorXsec:\n";
    std::cerr << "    Negative indicies not allowed.\n";
    return false;
  }
  acceptorElectronXsec[acceptorNumber] = eXsec; 
  acceptorHoleXsec[acceptorNumber] = hXsec; 

  validXsec = true;
  for(int i = 0; i<nDonor; i++){
    if(donorElectronXsec[i] < 0) validXsec = false;
    if(donorHoleXsec[i] < 0) validXsec = false;
  }
  for(int i = 0; i<nAcceptor; i++){
    if(acceptorElectronXsec[i] < 0) validXsec = false;
    if(acceptorHoleXsec[i] < 0) validXsec = false;
  }
  return true;
}

bool ComponentTcad2d::SetDonorConc(const int donorNumber, const double concentration){
  if (nDonor < 1) {
    std::cerr << m_className << "::SetDonorXsec:\n";
    std::cerr << "    No donor occupation maps exists.\n";
    return false;
  }
  if (donorNumber > nDonor - 1) {
    std::cerr << m_className << "::SetDonorXsec:\n";
    std::cerr << "    This donor occupation map does not exist.\n";
    return false;
  }
  if (donorNumber < 0) {
    std::cerr << m_className << "::SetDonorXsec:\n";
    std::cerr << "    Negative indicies not allowed.\n";
    return false;
  }
  donorConc[donorNumber] = concentration; 

  validConc = true;
  for(int i = 0; i<nDonor; i++){
    if(donorConc[i] < 0) validConc = false;
  }
  for(int i = 0; i<nAcceptor; i++){
    if(acceptorConc[i] < 0) validConc = false;
  }
  return true;
}

bool ComponentTcad2d::SetAcceptorConc(const int acceptorNumber, const double concentration){
  if (nAcceptor < 1) {
    std::cerr << m_className << "::SetAcceptorXsec:\n";
    std::cerr << "    No acceptor occupation maps exists.\n";
    return false;
  }
  if (acceptorNumber > nAcceptor - 1) {
    std::cerr << m_className << "::SetAcceptorXsec:\n";
    std::cerr << "    This acceptor occupation map does not exist.\n";
    return false;
  }
  if (acceptorNumber < 0) {
    std::cerr << m_className << "::SetAcceptorXsec:\n";
    std::cerr << "    Negative indicies not allowed.\n";
    return false;
  }
  acceptorConc[acceptorNumber] = concentration; 

  validConc = true;
  for(int i = 0; i<nDonor; i++){
    if(donorConc[i] < 0) validConc = false;
  }
  for(int i = 0; i<nAcceptor; i++){
    if(acceptorConc[i] < 0) validConc = false;
  }
  return true;
}

bool ComponentTcad2d::ElectronAttachment(const double x, const double y, const double z, double& eta){
  //Check for nTraps > 1, valid Xsec, valid Donor
  if(!validConc){
    std::cerr << m_className << "::ElectronAttachment:\n";
    std::cerr << "    Trap concentrations are invalid.\n";
    return false;
  }
  if(!validXsec){
    std::cerr << m_className << "::ElectronAttachment:\n";
    std::cerr << "    Trap cross-sections are invalid.\n";
    return false;
  }
  if(nAcceptor + nDonor < 1){
    std::cerr << m_className << "::ElectronAttachment:\n";
    std::cerr << "    This component has not traps.\n";
    return false;
  }
  eta = 0;
  double occupationFraction;
  for(int i = 0; i<nAcceptor; i++){
	  this->GetAcceptorOccupation(x, y, z, i, occupationFraction);
	  eta += acceptorConc[i]*acceptorElectronXsec[i]*(1-occupationFraction);
  }
  for(int i = 0; i<nDonor; i++){
	  this->GetDonorOccupation(x, y, z, i, occupationFraction);
	  eta += donorConc[i]*donorElectronXsec[i]*occupationFraction;
  }
  return true;
}

bool ComponentTcad2d::HoleAttachment(const double x, const double y, const double z,
                           double& eta){
  //Check for nTraps > 1, valid Xsec, valid Acceptor
  if(!validConc){
    std::cerr << m_className << "::HoleAttachment:\n";
    std::cerr << "    Trap concentrations are invalid.\n";
    return false;
  }
  if(!validXsec){
    std::cerr << m_className << "::HoleAttachment:\n";
    std::cerr << "    Trap cross-sections are invalid.\n";
    return false;
  }
  if(nAcceptor + nDonor < 1){
    std::cerr << m_className << "::HoleAttachment:\n";
    std::cerr << "    This component has not traps.\n";
    return false;
  }
  eta = 0;
  double occupationFraction;
  for(int i = 0; i<nAcceptor; i++){
	  this->GetAcceptorOccupation(x, y, z, i, occupationFraction);
	  eta += acceptorConc[i]*acceptorHoleXsec[i]*occupationFraction;
  }
  for(int i = 0; i<nDonor; i++){
	  this->GetDonorOccupation(x, y, z, i, occupationFraction);
	  eta += donorConc[i]*acceptorElectronXsec[i]*(1-occupationFraction);
  }
  return true;
}

void ComponentTcad2d::WeightingField(const double x, const double y, const double z,
                                   double& wx, double& wy, double& wz, 
				   const std::string& label) {
  int status = 0;
  Medium* med = NULL;
  double v = 0.;
  ElectricField(x, y, z, wx, wy, wz, v, med, status);
}

void ComponentTcad2d::ElectricField(const double xin, const double yin,
                                    const double zin, double& ex, double& ey,
                                    double& ez, double& p, Medium*& m,
                                    int& status) {

  m = NULL;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    status = -10;
    return;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  bool xMirrored = false;
  const double cellsx = xMaxBoundingBox - xMinBoundingBox;
  if (m_xPeriodic) {
    x = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (x < xMinBoundingBox) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (xNew < xMinBoundingBox) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = xMinBoundingBox + xMaxBoundingBox - xNew;
      xMirrored = true;
    }
    x = xNew;
  }
  bool yMirrored = false;
  const double cellsy = yMaxBoundingBox - yMinBoundingBox;
  if (m_yPeriodic) {
    y = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (y < yMinBoundingBox) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (yNew < yMinBoundingBox) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = yMinBoundingBox + yMaxBoundingBox - yNew;
      yMirrored = true;
    }
    y = yNew;
  }

  // Check if the point is inside the bounding box.
  if (x < xMinBoundingBox || x > xMaxBoundingBox || y < yMinBoundingBox ||
      y > yMaxBoundingBox) {
    status = -11;
    return;
  }
  if (hasRangeZ) {
    if (z < zMinBoundingBox || z > zMaxBoundingBox) {
      status = -11;
      return;
    }
  }

  // Initialise the electric field and potential.
  ex = ey = ez = p = 0.;
  // Assume this will work.
  status = 0;
  // Check if the point is still located in the previously found element.
  int i = lastElement;
  switch (elements[i].type) {
    case 1:
      if (CheckLine(x, y, i)) {
        ex = w[0] * vertices[elements[i].vertex[0]].ex +
             w[1] * vertices[elements[i].vertex[1]].ex;
        ey = w[0] * vertices[elements[i].vertex[0]].ey +
             w[1] * vertices[elements[i].vertex[1]].ey;
        p = w[0] * vertices[elements[i].vertex[0]].p +
            w[1] * vertices[elements[i].vertex[1]].p;
        if (xMirrored) ex = -ex;
        if (yMirrored) ey = -ey;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;
    case 2:
      if (CheckTriangle(x, y, i)) {
        ex = w[0] * vertices[elements[i].vertex[0]].ex +
             w[1] * vertices[elements[i].vertex[1]].ex +
             w[2] * vertices[elements[i].vertex[2]].ex;
        ey = w[0] * vertices[elements[i].vertex[0]].ey +
             w[1] * vertices[elements[i].vertex[1]].ey +
             w[2] * vertices[elements[i].vertex[2]].ey;
        p = w[0] * vertices[elements[i].vertex[0]].p +
            w[1] * vertices[elements[i].vertex[1]].p +
            w[2] * vertices[elements[i].vertex[2]].p;
        if (xMirrored) ex = -ex;
        if (yMirrored) ey = -ey;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;
    case 3:
      if (CheckRectangle(x, y, i)) {
        ex = w[0] * vertices[elements[i].vertex[0]].ex +
             w[1] * vertices[elements[i].vertex[1]].ex +
             w[2] * vertices[elements[i].vertex[2]].ex +
             w[3] * vertices[elements[i].vertex[3]].ex;
        ey = w[0] * vertices[elements[i].vertex[0]].ey +
             w[1] * vertices[elements[i].vertex[1]].ey +
             w[2] * vertices[elements[i].vertex[2]].ey +
             w[3] * vertices[elements[i].vertex[3]].ey;
        p = w[0] * vertices[elements[i].vertex[0]].p +
            w[1] * vertices[elements[i].vertex[1]].p +
            w[2] * vertices[elements[i].vertex[2]].p +
            w[3] * vertices[elements[i].vertex[3]].p;
        if (xMirrored) ex = -ex;
        if (yMirrored) ey = -ey;
        m = regions[elements[i].region].medium;
        if (!regions[elements[i].region].drift || m == 0) status = -5;
        return;
      }
      break;
    default:
      std::cerr << m_className << "::ElectricField:\n";
      std::cerr << "    Unknown element type (" << elements[i].type << ").\n";
      status = -11;
      return;
      break;
  }

  // The point is not in the previous element.
  // Check the adjacent elements.
  for (int j = elements[lastElement].nNeighbours; j--;) {
    i = elements[lastElement].neighbours[j];
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex +
               w[1] * vertices[elements[i].vertex[1]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey +
               w[1] * vertices[elements[i].vertex[1]].ey;
          p = w[0] * vertices[elements[i].vertex[0]].p +
              w[1] * vertices[elements[i].vertex[1]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex +
               w[1] * vertices[elements[i].vertex[1]].ex +
               w[2] * vertices[elements[i].vertex[2]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey +
               w[1] * vertices[elements[i].vertex[1]].ey +
               w[2] * vertices[elements[i].vertex[2]].ey;
          p = w[0] * vertices[elements[i].vertex[0]].p +
              w[1] * vertices[elements[i].vertex[1]].p +
              w[2] * vertices[elements[i].vertex[2]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex +
               w[1] * vertices[elements[i].vertex[1]].ex +
               w[2] * vertices[elements[i].vertex[2]].ex +
               w[3] * vertices[elements[i].vertex[3]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey +
               w[1] * vertices[elements[i].vertex[1]].ey +
               w[2] * vertices[elements[i].vertex[2]].ey +
               w[3] * vertices[elements[i].vertex[3]].ey;
          p = w[0] * vertices[elements[i].vertex[0]].p +
              w[1] * vertices[elements[i].vertex[1]].p +
              w[2] * vertices[elements[i].vertex[2]].p +
              w[3] * vertices[elements[i].vertex[3]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      default:
        std::cerr << m_className << "::ElectricField:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        status = -11;
        return;
        break;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  for (i = nElements; i--;) {
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex +
               w[1] * vertices[elements[i].vertex[1]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey +
               w[1] * vertices[elements[i].vertex[1]].ey;
          p = w[0] * vertices[elements[i].vertex[0]].p +
              w[1] * vertices[elements[i].vertex[1]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex +
               w[1] * vertices[elements[i].vertex[1]].ex +
               w[2] * vertices[elements[i].vertex[2]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey +
               w[1] * vertices[elements[i].vertex[1]].ey +
               w[2] * vertices[elements[i].vertex[2]].ey;
          p = w[0] * vertices[elements[i].vertex[0]].p +
              w[1] * vertices[elements[i].vertex[1]].p +
              w[2] * vertices[elements[i].vertex[2]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          ex = w[0] * vertices[elements[i].vertex[0]].ex +
               w[1] * vertices[elements[i].vertex[1]].ex +
               w[2] * vertices[elements[i].vertex[2]].ex +
               w[3] * vertices[elements[i].vertex[3]].ex;
          ey = w[0] * vertices[elements[i].vertex[0]].ey +
               w[1] * vertices[elements[i].vertex[1]].ey +
               w[2] * vertices[elements[i].vertex[2]].ey +
               w[3] * vertices[elements[i].vertex[3]].ey;
          p = w[0] * vertices[elements[i].vertex[0]].p +
              w[1] * vertices[elements[i].vertex[1]].p +
              w[2] * vertices[elements[i].vertex[2]].p +
              w[3] * vertices[elements[i].vertex[3]].p;
          if (xMirrored) ex = -ex;
          if (yMirrored) ey = -ey;
          lastElement = i;
          m = regions[elements[i].region].medium;
          if (!regions[elements[i].region].drift || m == 0) status = -5;
          return;
        }
        break;
      default:
        std::cerr << m_className << "::ElectricField:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        status = -11;
        return;
        break;
    }
  }
  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::ElectricField:\n";
    std::cerr << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  status = -6;
  return;
}

void ComponentTcad2d::ElectricField(const double x, const double y,
                                    const double z, double& ex, double& ey,
                                    double& ez, Medium*& m, int& status) {

  double v = 0.;
  ElectricField(x, y, z, ex, ey, ez, v, m, status);
}

Medium* ComponentTcad2d::GetMedium(const double xin, const double yin,
                                   const double zin) {

  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Field map not available for interpolation.\n";
    return NULL;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  const double cellsx = xMaxBoundingBox - xMinBoundingBox;
  if (m_xPeriodic) {
    x = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (x < xMinBoundingBox) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (xNew < xMinBoundingBox) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = xMinBoundingBox + xMaxBoundingBox - xNew;
    }
    x = xNew;
  }
  const double cellsy = yMaxBoundingBox - yMinBoundingBox;
  if (m_yPeriodic) {
    y = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (y < yMinBoundingBox) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (yNew < yMinBoundingBox) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = yMinBoundingBox + yMaxBoundingBox - yNew;
    }
    y = yNew;
  }

  // Check if the point is inside the bounding box.
  if (x < xMinBoundingBox || x > xMaxBoundingBox || y < yMinBoundingBox ||
      y > yMaxBoundingBox) {
    return NULL;
  }
  if (hasRangeZ) {
    if (z < zMinBoundingBox || z > zMaxBoundingBox) {
      return NULL;
    }
  }

  // Check if the point is still located in the previous element.
  int i = lastElement;
  switch (elements[i].type) {
    case 1:
      if (CheckLine(x, y, i)) {
        return regions[elements[i].region].medium;
      }
      break;
    case 2:
      if (CheckTriangle(x, y, i)) {
        return regions[elements[i].region].medium;
      }
      break;
    case 3:
      if (CheckRectangle(x, y, i)) {
        return regions[elements[i].region].medium;
      }
      break;
    default:
      std::cerr << m_className << "::GetMedium:\n";
      std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
      return NULL;
      break;
  }

  // The point is not in the previous element.
  // Check the adjacent elements.
  for (int j = elements[lastElement].nNeighbours; j--;) {
    i = elements[lastElement].neighbours[j];
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          lastElement = i;
          return regions[elements[i].region].medium;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          lastElement = i;
          return regions[elements[i].region].medium;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          lastElement = i;
          return regions[elements[i].region].medium;
        }
        break;
      default:
        std::cerr << m_className << "::GetMedium:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return NULL;
        break;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  for (i = nElements; i--;) {
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          lastElement = i;
          return regions[elements[i].region].medium;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          lastElement = i;
          return regions[elements[i].region].medium;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          lastElement = i;
          return regions[elements[i].region].medium;
        }
        break;
      default:
        std::cerr << m_className << "::GetMedium:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return NULL;
        break;
    }
  }
  // The point is outside the mesh.
  return NULL;
}

bool ComponentTcad2d::GetMobility(const double xin, const double yin,
                                  const double zin, double& emob,
                                  double& hmob) {

  emob = hmob = 0.;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetMobility:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  const double cellsx = xMaxBoundingBox - xMinBoundingBox;
  if (m_xPeriodic) {
    x = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (x < xMinBoundingBox) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (xNew < xMinBoundingBox) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = xMinBoundingBox + xMaxBoundingBox - xNew;
    }
    x = xNew;
  }
  const double cellsy = xMaxBoundingBox - xMinBoundingBox;
  if (m_yPeriodic) {
    y = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (y < yMinBoundingBox) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (yNew < yMinBoundingBox) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = yMinBoundingBox + yMaxBoundingBox - yNew;
    }
    y = yNew;
  }

  // Check if the point is inside the bounding box.
  if (x < xMinBoundingBox || x > xMaxBoundingBox || y < yMinBoundingBox ||
      y > yMaxBoundingBox) {
    return false;
  }
  if (hasRangeZ) {
    if (z < zMinBoundingBox || z > zMaxBoundingBox) {
      return false;
    }
  }

  // Check if the point is still located in the previously found element.
  int i = lastElement;
  switch (elements[i].type) {
    case 1:
      if (CheckLine(x, y, i)) {
        emob = w[0] * vertices[elements[i].vertex[0]].emob +
               w[1] * vertices[elements[i].vertex[1]].emob;
        hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
               w[1] * vertices[elements[i].vertex[1]].hmob;
        return true;
      }
      break;
    case 2:
      if (CheckTriangle(x, y, i)) {
        emob = w[0] * vertices[elements[i].vertex[0]].emob +
               w[1] * vertices[elements[i].vertex[1]].emob +
               w[2] * vertices[elements[i].vertex[2]].emob;
        hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
               w[1] * vertices[elements[i].vertex[1]].hmob +
               w[2] * vertices[elements[i].vertex[2]].hmob;
        return true;
      }
      break;
    case 3:
      if (CheckRectangle(x, y, i)) {
        emob = w[0] * vertices[elements[i].vertex[0]].emob +
               w[1] * vertices[elements[i].vertex[1]].emob +
               w[2] * vertices[elements[i].vertex[2]].emob +
               w[3] * vertices[elements[i].vertex[3]].emob;
        hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
               w[1] * vertices[elements[i].vertex[1]].hmob +
               w[2] * vertices[elements[i].vertex[2]].hmob +
               w[3] * vertices[elements[i].vertex[3]].hmob;
        return true;
      }
      break;
    default:
      std::cerr << m_className << "::GetMobility:\n";
      std::cerr << "    Unknown element type (" << elements[i].type << ").\n";
      return false;
      break;
  }

  // The point is not in the previous element.
  // Check the adjacent elements.
  for (int j = elements[lastElement].nNeighbours; j--;) {
    i = elements[lastElement].neighbours[j];
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          emob = w[0] * vertices[elements[i].vertex[0]].emob +
                 w[1] * vertices[elements[i].vertex[1]].emob;
          hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
                 w[1] * vertices[elements[i].vertex[1]].hmob;
          lastElement = i;
          return true;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          emob = w[0] * vertices[elements[i].vertex[0]].emob +
                 w[1] * vertices[elements[i].vertex[1]].emob +
                 w[2] * vertices[elements[i].vertex[2]].emob;
          hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
                 w[1] * vertices[elements[i].vertex[1]].hmob +
                 w[2] * vertices[elements[i].vertex[2]].hmob;
          lastElement = i;
          return true;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          emob = w[0] * vertices[elements[i].vertex[0]].emob +
                 w[1] * vertices[elements[i].vertex[1]].emob +
                 w[2] * vertices[elements[i].vertex[2]].emob +
                 w[3] * vertices[elements[i].vertex[3]].emob;
          hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
                 w[1] * vertices[elements[i].vertex[1]].hmob +
                 w[2] * vertices[elements[i].vertex[2]].hmob +
                 w[3] * vertices[elements[i].vertex[3]].hmob;
          lastElement = i;
          return true;
        }
        break;
      default:
        std::cerr << m_className << "::GetMobility:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return false;
        break;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  for (i = nElements; i--;) {
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          emob = w[0] * vertices[elements[i].vertex[0]].emob +
                 w[1] * vertices[elements[i].vertex[1]].emob;
          hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
                 w[1] * vertices[elements[i].vertex[1]].hmob;
          lastElement = i;
          return true;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          emob = w[0] * vertices[elements[i].vertex[0]].emob +
                 w[1] * vertices[elements[i].vertex[1]].emob +
                 w[2] * vertices[elements[i].vertex[2]].emob;
          hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
                 w[1] * vertices[elements[i].vertex[1]].hmob +
                 w[2] * vertices[elements[i].vertex[2]].hmob;
          lastElement = i;
          return true;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          emob = w[0] * vertices[elements[i].vertex[0]].emob +
                 w[1] * vertices[elements[i].vertex[1]].emob +
                 w[2] * vertices[elements[i].vertex[2]].emob +
                 w[3] * vertices[elements[i].vertex[3]].emob;
          hmob = w[0] * vertices[elements[i].vertex[0]].hmob +
                 w[1] * vertices[elements[i].vertex[1]].hmob +
                 w[2] * vertices[elements[i].vertex[2]].hmob +
                 w[3] * vertices[elements[i].vertex[3]].hmob;
          lastElement = i;
          return true;
        }
        break;
      default:
        std::cerr << m_className << "::GetMobility:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return false;
        break;
    }
  }
  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetMobility:\n";
    std::cerr << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;
}

bool ComponentTcad2d::GetDonorOccupation(const double xin, const double yin,
                                  const double zin, int donorNumber ,
                                  double& occupationFraction ) {
  if (donorNumber > nDonor) {
    std::cerr << m_className << "::GetDonorOccupation:\n";
    std::cerr << "    This donor does not exist.\n";
    return false;
  }

  occupationFraction = 0;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetDonorOccupation:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  const double cellsx = xMaxBoundingBox - xMinBoundingBox;
  if (m_xPeriodic) {
    x = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (x < xMinBoundingBox) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (xNew < xMinBoundingBox) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = xMinBoundingBox + xMaxBoundingBox - xNew;
    }
    x = xNew;
  }
  const double cellsy = xMaxBoundingBox - xMinBoundingBox;
  if (m_yPeriodic) {
    y = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (y < yMinBoundingBox) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (yNew < yMinBoundingBox) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = yMinBoundingBox + yMaxBoundingBox - yNew;
    }
    y = yNew;
  }

  // Check if the point is inside the bounding box.
  if (x < xMinBoundingBox || x > xMaxBoundingBox || y < yMinBoundingBox ||
      y > yMaxBoundingBox) {
    return false;
  }
  if (hasRangeZ) {
    if (z < zMinBoundingBox || z > zMaxBoundingBox) {
      return false;
    }
  }

  // Check if the point is still located in the previously found element.
  int i = lastElement;
  switch (elements[i].type) {
    case 1:
      if (CheckLine(x, y, i)) {
        occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
               w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber];
        return true;
      }
      break;
    case 2:
      if (CheckTriangle(x, y, i)) {
        occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
               w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber] +
               w[2] * vertices[elements[i].vertex[2]].donorOcc[donorNumber];
        return true;
      }
      break;
    case 3:
      if (CheckRectangle(x, y, i)) {
        occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
               w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber] +
               w[2] * vertices[elements[i].vertex[2]].donorOcc[donorNumber] +
               w[3] * vertices[elements[i].vertex[3]].donorOcc[donorNumber];
        return true;
      }
      break;
    default:
      std::cerr << m_className << "::GetDonorOccupation:\n";
      std::cerr << "    Unknown element type (" << elements[i].type << ").\n";
      return false;
      break;
  }

  // The point is not in the previous element.
  // Check the adjacent elements.
  for (int j = elements[lastElement].nNeighbours; j--;) {
    i = elements[lastElement].neighbours[j];
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].donorOcc[donorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].donorOcc[donorNumber] +
                 w[3] * vertices[elements[i].vertex[3]].donorOcc[donorNumber];
          lastElement = i;
          return true;
        }
        break;
      default:
        std::cerr << m_className << "::GetDonorOccupation:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return false;
        break;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  for (i = nElements; i--;) {
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].donorOcc[donorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].donorOcc[donorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].donorOcc[donorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].donorOcc[donorNumber] +
                 w[3] * vertices[elements[i].vertex[3]].donorOcc[donorNumber];
          lastElement = i;
          return true;
        }
        break;
      default:
        std::cerr << m_className << "::GetDonorOccupation:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return false;
        break;
    }
  }
  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetDonorOccupation:\n";
    std::cerr << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;
}

bool ComponentTcad2d::GetAcceptorOccupation(const double xin, const double yin,
                                  const double zin, int acceptorNumber,
                                  double& occupationFraction ) {
  if (acceptorNumber > nAcceptor) {
    std::cerr << m_className << "::GetAcceptorOccupation:\n";
    std::cerr << "    This acceptor does not exist.\n";
    return false;
  }

  occupationFraction = 0;
  // Make sure the field map has been loaded.
  if (!m_ready) {
    std::cerr << m_className << "::GetAcceptorOccupation:\n";
    std::cerr << "    Field map is not available for interpolation.\n";
    return false;
  }

  double x = xin, y = yin, z = zin;
  // In case of periodicity, reduce to the cell volume.
  const double cellsx = xMaxBoundingBox - xMinBoundingBox;
  if (m_xPeriodic) {
    x = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (x < xMinBoundingBox) x += cellsx;
  } else if (m_xMirrorPeriodic) {
    double xNew = xMinBoundingBox + fmod(x - xMinBoundingBox, cellsx);
    if (xNew < xMinBoundingBox) xNew += cellsx;
    int nx = int(floor(0.5 + (xNew - x) / cellsx));
    if (nx != 2 * (nx / 2)) {
      xNew = xMinBoundingBox + xMaxBoundingBox - xNew;
    }
    x = xNew;
  }
  const double cellsy = xMaxBoundingBox - xMinBoundingBox;
  if (m_yPeriodic) {
    y = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (y < yMinBoundingBox) y += cellsy;
  } else if (m_yMirrorPeriodic) {
    double yNew = yMinBoundingBox + fmod(y - yMinBoundingBox, cellsy);
    if (yNew < yMinBoundingBox) yNew += cellsy;
    int ny = int(floor(0.5 + (yNew - y) / cellsy));
    if (ny != 2 * (ny / 2)) {
      yNew = yMinBoundingBox + yMaxBoundingBox - yNew;
    }
    y = yNew;
  }

  // Check if the point is inside the bounding box.
  if (x < xMinBoundingBox || x > xMaxBoundingBox || y < yMinBoundingBox ||
      y > yMaxBoundingBox) {
    return false;
  }
  if (hasRangeZ) {
    if (z < zMinBoundingBox || z > zMaxBoundingBox) {
      return false;
    }
  }

  // Check if the point is still located in the previously found element.
  int i = lastElement;
  switch (elements[i].type) {
    case 1:
      if (CheckLine(x, y, i)) {
        occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
               w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber];
        return true;
      }
      break;
    case 2:
      if (CheckTriangle(x, y, i)) {
        occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
               w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber] +
               w[2] * vertices[elements[i].vertex[2]].acceptorOcc[acceptorNumber];
        return true;
      }
      break;
    case 3:
      if (CheckRectangle(x, y, i)) {
        occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
               w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber] +
               w[2] * vertices[elements[i].vertex[2]].acceptorOcc[acceptorNumber] +
               w[3] * vertices[elements[i].vertex[3]].acceptorOcc[acceptorNumber];
        return true;
      }
      break;
    default:
      std::cerr << m_className << "::GetAcceptorOccupation:\n";
      std::cerr << "    Unknown element type (" << elements[i].type << ").\n";
      return false;
      break;
  }

  // The point is not in the previous element.
  // Check the adjacent elements.
  for (int j = elements[lastElement].nNeighbours; j--;) {
    i = elements[lastElement].neighbours[j];
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].acceptorOcc[acceptorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].acceptorOcc[acceptorNumber] +
                 w[3] * vertices[elements[i].vertex[3]].acceptorOcc[acceptorNumber];
          lastElement = i;
          return true;
        }
        break;
      default:
        std::cerr << m_className << "::GetAcceptorOccupation:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return false;
        break;
    }
  }

  // The point is not in the previous element nor in the adjacent ones.
  // We have to loop over all elements.
  for (i = nElements; i--;) {
    if (x < vertices[elements[i].vertex[0]].x) continue;
    switch (elements[i].type) {
      case 1:
        if (CheckLine(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 2:
        if (CheckTriangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].acceptorOcc[acceptorNumber];
          lastElement = i;
          return true;
        }
        break;
      case 3:
        if (CheckRectangle(x, y, i)) {
          occupationFraction = w[0] * vertices[elements[i].vertex[0]].acceptorOcc[acceptorNumber] +
                 w[1] * vertices[elements[i].vertex[1]].acceptorOcc[acceptorNumber] +
                 w[2] * vertices[elements[i].vertex[2]].acceptorOcc[acceptorNumber] +
                 w[3] * vertices[elements[i].vertex[3]].acceptorOcc[acceptorNumber];
          lastElement = i;
          return true;
        }
        break;
      default:
        std::cerr << m_className << "::GetAcceptorOccupation:\n";
        std::cerr << "    Invalid element type (" << elements[i].type << ").\n";
        return false;
        break;
    }
  }
  // Point is outside the mesh.
  if (m_debug) {
    std::cerr << m_className << "::GetAcceptorOccupation:\n";
    std::cerr << "    Point (" << x << ", " << y << ") is outside the mesh.\n";
  }
  return false;
}

bool ComponentTcad2d::Initialise(const std::string gridfilename,
                                 const std::string datafilename) {

  m_ready = false;
  // Import mesh data from .grd file.
  if (!LoadGrid(gridfilename)) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Importing mesh data failed.\n";
    return false;
  }

  hasPotential = hasField = false;
  hasElectronMobility = hasHoleMobility = false;
  nDonor = nAcceptor = 0;

  // Import electric field, potential, mobilities and trap occupation values from .dat file.
  if (!LoadData(datafilename)) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    Importing electric field and potential failed.\n";
    return false;
  }

  // Find min./max. coordinates and potentials.
  xMaxBoundingBox = xMinBoundingBox = vertices[elements[0].vertex[0]].x;
  yMaxBoundingBox = yMinBoundingBox = vertices[elements[0].vertex[0]].y;
  pMax = pMin = vertices[elements[0].vertex[0]].p;
  for (int i = nElements; i--;) {
    for (int j = 0; j <= elements[i].type; ++j) {
      if (vertices[elements[i].vertex[j]].x < xMinBoundingBox) {
        xMinBoundingBox = vertices[elements[i].vertex[j]].x;
      } else if (vertices[elements[i].vertex[j]].x > xMaxBoundingBox) {
        xMaxBoundingBox = vertices[elements[i].vertex[j]].x;
      }
      if (vertices[elements[i].vertex[j]].y < yMinBoundingBox) {
        yMinBoundingBox = vertices[elements[i].vertex[j]].y;
      } else if (vertices[elements[i].vertex[j]].y > yMaxBoundingBox) {
        yMaxBoundingBox = vertices[elements[i].vertex[j]].y;
      }
      if (vertices[elements[i].vertex[j]].p < pMin) {
        pMin = vertices[elements[i].vertex[j]].p;
      } else if (vertices[elements[i].vertex[j]].p > pMax) {
        pMax = vertices[elements[i].vertex[j]].p;
      }
    }
  }

  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Available data:\n";
  if (hasPotential) {
    std::cout << "      Electrostatic potential\n";
  }
  if (hasField) {
    std::cout << "      Electric field\n";
  }
  if (hasElectronMobility) {
    std::cout << "      Electron mobility\n";
  }
  if (hasHoleMobility) {
    std::cout << "      Hole mobility\n";
  }
  if (nDonor > 0){
      std::cout << "      Donor trap occupation maps: " << nDonor <<"\n";
  }
  if (nAcceptor > 0){
      std::cout << "      Acceptor trap occupation maps: " << nAcceptor <<"\n";
  }
  std::cout << "    Bounding box:\n";
  std::cout << "      " << xMinBoundingBox << " < x [cm] < " << xMaxBoundingBox
            << "\n";
  std::cout << "      " << yMinBoundingBox << " < y [cm] < " << yMaxBoundingBox
            << "\n";
  std::cout << "    Voltage range:\n";
  std::cout << "      " << pMin << " < V < " << pMax << "\n";

  bool ok = true;

  // Count the number of elements belonging to a region.
  std::vector<int> nElementsRegion;
  nElementsRegion.resize(nRegions);
  for (int i = nRegions; i--;) nElementsRegion[i] = 0;

  // Count the different element shapes.
  int nLines = 0;
  int nTriangles = 0;
  int nRectangles = 0;
  int nOtherShapes = 0;

  // Check if there are elements which are not part of any region.
  int nLoose = 0;
  std::vector<int> looseElements;
  looseElements.clear();

  // Check if there are degenerate elements.
  int nDegenerate = 0;
  std::vector<int> degenerateElements;
  degenerateElements.clear();

  for (int i = nElements; i--;) {
    if (elements[i].type == 1) {
      ++nLines;
      if (elements[i].vertex[0] == elements[i].vertex[1]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else if (elements[i].type == 2) {
      ++nTriangles;
      if (elements[i].vertex[0] == elements[i].vertex[1] ||
          elements[i].vertex[1] == elements[i].vertex[2] ||
          elements[i].vertex[2] == elements[i].vertex[0]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else if (elements[i].type == 3) {
      ++nRectangles;
      if (elements[i].vertex[0] == elements[i].vertex[1] ||
          elements[i].vertex[0] == elements[i].vertex[2] ||
          elements[i].vertex[0] == elements[i].vertex[3] ||
          elements[i].vertex[1] == elements[i].vertex[2] ||
          elements[i].vertex[1] == elements[i].vertex[3] ||
          elements[i].vertex[2] == elements[i].vertex[3]) {
        degenerateElements.push_back(i);
        ++nDegenerate;
      }
    } else {
      // Other shapes should not occur, since they were excluded in LoadGrid.
      ++nOtherShapes;
    }
    if (elements[i].region >= 0 && elements[i].region < nRegions) {
      ++nElementsRegion[elements[i].region];
    } else {
      looseElements.push_back(i);
      ++nLoose;
    }
  }

  if (nDegenerate > 0) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    The following elements are degenerate:\n";
    for (int i = nDegenerate; i--;) {
      std::cerr << "      " << degenerateElements[i] << "\n";
    }
    ok = false;
  }

  if (nLoose > 0) {
    std::cerr << m_className << "::Initialise:\n";
    std::cerr << "    The following elements are not part of any region:\n";
    for (int i = nLoose; i--;) {
      std::cerr << "      " << looseElements[i] << "\n";
    }
    ok = false;
  }

  std::cout << m_className << "::Initialise:\n";
  std::cout << "    Number of regions: " << nRegions << "\n";
  for (int i = 0; i < nRegions; ++i) {
    std::cout << "      " << i << ": " << regions[i].name << ", "
              << nElementsRegion[i] << " elements\n";
  }

  std::cout << "    Number of elements: " << nElements << "\n";
  if (nLines > 0) {
    std::cout << "      " << nLines << " lines\n";
  }
  if (nTriangles > 0) {
    std::cout << "      " << nTriangles << " triangles\n";
  }
  if (nRectangles > 0) {
    std::cout << "      " << nRectangles << " rectangles\n";
  }
  if (nOtherShapes > 0) {
    std::cout << "      " << nOtherShapes << " elements of unknown type\n";
    std::cout << "      Program bug!\n";
    m_ready = false;
    Cleanup();
    return false;
  }

  std::cout << "    Number of vertices: " << nVertices << "\n";

  // Find adjacent elements.
  FindNeighbours();

  if (!ok) {
    m_ready = false;
    Cleanup();
    return false;
  }

  m_ready = true;
  UpdatePeriodicity();
  return true;
}

bool ComponentTcad2d::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                     double& xmax, double& ymax, double& zmax) {

  if (!m_ready) return false;
  if (m_xPeriodic || m_xMirrorPeriodic) {
    xmin = -INFINITY;
    xmax = +INFINITY;
  } else {
    xmin = xMinBoundingBox;
    xmax = xMaxBoundingBox;
  }

  if (m_yPeriodic || m_yMirrorPeriodic) {
    ymin = -INFINITY;
    ymax = +INFINITY;
  } else {
    ymin = yMinBoundingBox;
    ymax = yMaxBoundingBox;
  }

  if (hasRangeZ) {
    zmin = zMinBoundingBox;
    zmax = zMaxBoundingBox;
  }
  return true;
}

void ComponentTcad2d::SetRangeZ(const double zmin, const double zmax) {

  if (fabs(zmax - zmin) <= 0.) {
    std::cerr << m_className << "::SetRangeZ:\n";
    std::cerr << "    Zero range is not permitted.\n";
    return;
  }
  zMinBoundingBox = std::min(zmin, zmax);
  zMaxBoundingBox = std::max(zmin, zmax);
  hasRangeZ = true;
}

bool ComponentTcad2d::GetVoltageRange(double& vmin, double& vmax) {

  if (!m_ready) return false;
  vmin = pMin;
  vmax = pMax;
  return true;
}

void ComponentTcad2d::PrintRegions() {

  // Do not proceed if not properly initialised.
  if (!m_ready) {
    std::cerr << m_className << "::PrintRegions:\n";
    std::cerr << "    Field map not yet initialised.\n";
    return;
  }

  if (nRegions < 0) {
    std::cerr << m_className << "::PrintRegions:\n";
    std::cerr << "    No regions are currently defined.\n";
    return;
  }

  std::cout << m_className << "::PrintRegions:\n";
  std::cout << "    Currently " << nRegions << " regions are defined.\n";
  std::cout << "      Index  Name       Medium\n";
  for (int i = 0; i < nRegions; ++i) {
    std::cout << "      " << i << "  " << regions[i].name;
    if (regions[i].medium == 0) {
      std::cout << "      none  ";
    } else {
      std::cout << "      " << regions[i].medium->GetName();
    }
    if (regions[i].drift) {
      std::cout << " (active region)\n";
    } else {
      std::cout << "\n";
    }
  }
}

void ComponentTcad2d::GetRegion(const int i, std::string& name, bool& active) {

  if (i < 0 || i >= nRegions) {
    std::cerr << m_className << "::GetRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  name = regions[i].name;
  active = regions[i].drift;
}

void ComponentTcad2d::SetDriftRegion(const int i) {

  if (i < 0 || i >= nRegions) {
    std::cerr << m_className << "::SetDriftRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  regions[i].drift = true;
}

void ComponentTcad2d::UnsetDriftRegion(const int i) {

  if (i < 0 || i >= nRegions) {
    std::cerr << m_className << "::UnsetDriftRegion:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }
  regions[i].drift = false;
}

void ComponentTcad2d::SetMedium(const int i, Medium* medium) {

  if (i < 0 || i >= nRegions) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return;
  }

  if (medium == 0) {
    std::cerr << m_className << "::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  regions[i].medium = medium;
}

Medium* ComponentTcad2d::GetMedium(const unsigned int& i) const {

  if (i >= (unsigned int)nRegions) {
    std::cerr << m_className << "::GetMedium:\n";
    std::cerr << "    Region " << i << " does not exist.\n";
    return NULL;
  }

  return regions[i].medium;
}

bool ComponentTcad2d::GetElement(const int i, double& vol, double& dmin,
                                 double& dmax, int& type) {

  if (i < 0 || i >= nElements) {
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Element index (" << i << ") out of range.\n";
    return false;
  }

  type = elements[i].type;
  if (elements[i].type == 1) {
    const double d = sqrt(pow(vertices[elements[i].vertex[1]].x -
                                  vertices[elements[i].vertex[0]].x,
                              2) +
                          pow(vertices[elements[i].vertex[1]].y -
                                  vertices[elements[i].vertex[0]].y,
                              2));
    dmin = dmax = vol = d;
  } else if (elements[i].type == 2) {
    vol = fabs((vertices[elements[i].vertex[2]].x -
                vertices[elements[i].vertex[0]].x) *
                   (vertices[elements[i].vertex[1]].y -
                    vertices[elements[i].vertex[0]].y) -
               (vertices[elements[i].vertex[2]].y -
                vertices[elements[i].vertex[0]].y) *
                   (vertices[elements[i].vertex[1]].x -
                    vertices[elements[i].vertex[0]].x)) /
          2.;
    const double a = sqrt(pow(vertices[elements[i].vertex[1]].x -
                                  vertices[elements[i].vertex[0]].x,
                              2) +
                          pow(vertices[elements[i].vertex[1]].y -
                                  vertices[elements[i].vertex[0]].y,
                              2));
    const double b = sqrt(pow(vertices[elements[i].vertex[2]].x -
                                  vertices[elements[i].vertex[0]].x,
                              2) +
                          pow(vertices[elements[i].vertex[2]].y -
                                  vertices[elements[i].vertex[0]].y,
                              2));
    const double c = sqrt(pow(vertices[elements[i].vertex[1]].x -
                                  vertices[elements[i].vertex[2]].x,
                              2) +
                          pow(vertices[elements[i].vertex[1]].y -
                                  vertices[elements[i].vertex[2]].y,
                              2));
    dmin = dmax = a;
    if (b > dmax) dmax = b;
    if (c > dmax) dmax = c;
    if (b < dmin) dmin = b;
    if (c < dmin) dmin = c;
  } else if (elements[i].type == 3) {
    const double a = sqrt(pow(vertices[elements[i].vertex[1]].x -
                                  vertices[elements[i].vertex[0]].x,
                              2) +
                          pow(vertices[elements[i].vertex[1]].y -
                                  vertices[elements[i].vertex[0]].y,
                              2));
    const double b = sqrt(pow(vertices[elements[i].vertex[3]].x -
                                  vertices[elements[i].vertex[0]].x,
                              2) +
                          pow(vertices[elements[i].vertex[3]].y -
                                  vertices[elements[i].vertex[0]].y,
                              2));
    vol = a * b;
    dmin = std::min(a, b);
    dmax = sqrt(a * a + b * b);
  } else {
    std::cerr << m_className << "::GetElement:\n";
    std::cerr << "    Unexpected element type (" << type << ")\n";
    return false;
  }
  return true;
}

bool ComponentTcad2d::GetElement(const int i, double& vol, double& dmin,
                                 double& dmax, int& type, int& node1,
                                 int& node2, int& node3, int& node4, int& reg) {

  if (!GetElement(i, vol, dmin, dmax, type)) return false;
  node1 = elements[i].vertex[0];
  node2 = elements[i].vertex[1];
  node3 = elements[i].vertex[2];
  node4 = elements[i].vertex[3];
  reg = elements[i].region;
  return true;
}

bool ComponentTcad2d::GetNode(const int i, double& x, double& y, double& v,
                              double& ex, double& ey) {

  if (i < 0 || i >= nVertices) {
    std::cerr << m_className << "::GetNode:\n";
    std::cerr << "    Node index (" << i << ") out of range.\n";
    return false;
  }

  x = vertices[i].x;
  y = vertices[i].y;
  v = vertices[i].p;
  ex = vertices[i].ex;
  ey = vertices[i].ey;
  return true;
}

bool ComponentTcad2d::LoadData(const std::string datafilename) {

  std::ifstream datafile;
  datafile.open(datafilename.c_str(), std::ios::in);
  if (!datafile) {
    std::cerr << m_className << "::LoadData:\n";
    std::cerr << "    Could not open file " << datafilename << ".\n";
    return false;
  }

  std::string line;
  std::istringstream data;

  std::vector<bool> isInRegion(nVertices);
  std::vector<int> fillCount(nVertices);

  for (int i = nVertices; i--;) {
    fillCount[i] = 0;
    vertices[i].p = 0.;
    vertices[i].ex = 0.;
    vertices[i].ey = 0.;
    vertices[i].emob = 0.;
    vertices[i].hmob = 0.;
    vertices[i].donorOcc.clear();
    vertices[i].acceptorOcc.clear();
    vertices[i].isShared = false;
  }
  donorElectronXsec.clear();
  donorHoleXsec.clear();
  acceptorElectronXsec.clear();
  acceptorHoleXsec.clear();
  donorConc.clear();
  acceptorConc.clear();

  std::string::size_type pBra, pKet, pEq;

  while (!datafile.fail()) {
    // Read one line.
    std::getline(datafile, line);
    // Strip white space from beginning of line.
    line.erase(line.begin(),
               std::find_if(line.begin(), line.end(),
                            not1(std::ptr_fun<int, int>(isspace))));
    // Find data section.
    if (line.substr(0, 8) == "function") {
      // Read type of data set.
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" found.
        std::cerr << m_className << "::LoadData:\n";
        std::cerr << "    Error reading file " << datafilename << ".\n";
        std::cerr << "    Line:\n";
        std::cerr << "    " << line << "\n";
        datafile.close();
        Cleanup();
        return false;
      }
      line = line.substr(pEq + 1);
      std::string dataset;
      data.str(line);
      data >> dataset;
      data.clear();
      if (dataset == "ElectrostaticPotential") {
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        // Get the region name (given in brackets).
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        // Check if the region name matches one from the mesh file.
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        // Get the number of values.
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // Mark the vertices belonging to this region.
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          // Read the next value.
          datafile >> val;
          // Find the next vertex belonging to the region.
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          // Check if there is a mismatch between the number of vertices
          // and the number of potential values.
          if (ivertex >= nVertices) {
            std::cerr << m_className << "::LoadData:\n";
            std::cerr << "    Error reading file " << datafilename << "\n";
            std::cerr << "    Dataset ElectrostaticPotential:\n";
            std::cerr << "      More values than vertices in region " << name
                      << "\n";
            datafile.close();
            Cleanup();
            return false;
          }
          vertices[ivertex].p = val;
          ++fillCount[ivertex];
          ++ivertex;
        }
        hasPotential = true;
      } else if (dataset == "ElectricField") {
        // Same procedure as for the potential.
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << ".\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // In case of the electric field, there are two values per vertex.
        nValues = nValues / 2;
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val1, val2;
        for (int j = 0; j < nValues; ++j) {
          datafile >> val1 >> val2;
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          if (ivertex >= nVertices) {
            std::cerr << m_className << "::LoadData\n"
                      << "    Error reading file " << datafilename << "\n";
            std::cerr << "    Dataset ElectricField:\n";
            std::cerr << "      More values than vertices in region " << name
                      << "\n";
            datafile.close();
            Cleanup();
            return false;
          }
          vertices[ivertex].ex = val1;
          vertices[ivertex].ey = val2;
          ++ivertex;
        }
        hasField = true;
      } else if (dataset == "eMobility") {
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        // Get the region name (given in brackets).
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        // Check if the region name matches one from the mesh file.
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        // Get the number of values.
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // Mark the vertices belonging to this region.
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          // Read the next value.
          datafile >> val;
          // Find the next vertex belonging to the region.
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          // Check if there is a mismatch between the number of vertices
          // and the number of mobility values.
          if (ivertex >= nVertices) {
            std::cerr << m_className << "::LoadData:\n";
            std::cerr << "    Dataset eMobility:\n";
            std::cerr << "      More values than vertices in region " << name
                      << "\n";
            datafile.close();
            Cleanup();
            return false;
          }
          // Convert from cm2 / (V s) to cm2 / (V ns).
          vertices[ivertex].emob = val * 1.e-9;
          ++fillCount[ivertex];
          ++ivertex;
        }
        hasElectronMobility = true;
      } else if (dataset == "hMobility") {
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        // Get the region name (given in brackets).
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        // Check if the region name matches one from the mesh file.
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        // Get the number of values.
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // Mark the vertices belonging to this region.
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          // Read the next value.
          datafile >> val;
          // Find the next vertex belonging to the region.
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          // Check if there is a mismatch between the number of vertices
          // and the number of mobility values.
          if (ivertex >= nVertices) {
            std::cerr << m_className << "::LoadData:\n";
            std::cerr << "    Dataset hMobility:\n";
            std::cerr << "      More values than vertices in region " << name
                      << "\n";
            datafile.close();
            Cleanup();
            return false;
          }
          // Convert from cm2 / (V s) to cm2 / (V ns).
          vertices[ivertex].hmob = val * 1.e-9;
          ++fillCount[ivertex];
          ++ivertex;
        }
        hasHoleMobility = true;
      } else if (dataset.substr(0,14) == "TrapOccupation" && dataset.substr(17,2) == "Do") {
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        // Get the region name (given in brackets).
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        // Check if the region name matches one from the mesh file.
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        // Get the number of values.
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // Mark the vertices belonging to this region.
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          // Read the next value.
          datafile >> val;
          // Find the next vertex belonging to the region.
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          // Check if there is a mismatch between the number of vertices
          // and the number of trap occupation values.
          if (ivertex >= nVertices) {
            std::cerr << m_className << "::LoadData:\n";
            std::cerr << "    Dataset DonorOccupation:\n";
            std::cerr << "      More values than vertices in region " << name
                      << "\n";
            datafile.close();
            Cleanup();
            return false;
          }
          vertices[ivertex].donorOcc.push_back(val);
          ++fillCount[ivertex];
          ++ivertex;
        }
	donorElectronXsec.push_back(-1);
	donorHoleXsec.push_back(-1);
	donorConc.push_back(-1);
        nDonor++;
      } else if (dataset.substr(0,14) == "TrapOccupation" && dataset.substr(17, 2) == "Ac") {
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        std::getline(datafile, line);
        // Get the region name (given in brackets).
        pBra = line.find('[');
        pKet = line.find(']');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        std::string name;
        data.str(line);
        data >> name;
        data.clear();
        // Check if the region name matches one from the mesh file.
        int index = -1;
        for (int j = 0; j < nRegions; ++j) {
          if (name == regions[j].name) {
            index = j;
            break;
          }
        }
        if (index == -1) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Unknown region " << name << ".\n";
          continue;
        }
        // Get the number of values.
        std::getline(datafile, line);
        pBra = line.find('(');
        pKet = line.find(')');
        if (pKet < pBra || pBra == std::string::npos ||
            pKet == std::string::npos) {
          std::cerr << m_className << "::LoadData:\n";
          std::cerr << "    Error reading file " << datafilename << "\n";
          std::cerr << "    Line:\n";
          std::cerr << "    " << line << "\n";
          datafile.close();
          Cleanup();
          return false;
        }
        line = line.substr(pBra + 1, pKet - pBra - 1);
        int nValues;
        data.str(line);
        data >> nValues;
        data.clear();
        // Mark the vertices belonging to this region.
        for (int j = nVertices; j--;) isInRegion[j] = false;
        for (int j = 0; j < nElements; ++j) {
          if (elements[j].region != index) continue;
          for (int k = 0; k <= elements[j].type; ++k) {
            isInRegion[elements[j].vertex[k]] = true;
          }
        }
        int ivertex = 0;
        double val;
        for (int j = 0; j < nValues; ++j) {
          // Read the next value.
          datafile >> val;
          // Find the next vertex belonging to the region.
          while (ivertex < nVertices) {
            if (isInRegion[ivertex]) break;
            ++ivertex;
          }
          // Check if there is a mismatch between the number of vertices
          // and the number of trap occupation values.
          if (ivertex >= nVertices) {
            std::cerr << m_className << "::LoadData:\n";
            std::cerr << "    Dataset DonorOccupation:\n";
            std::cerr << "      More values than vertices in region " << name
                      << "\n";
            datafile.close();
            Cleanup();
            return false;
          }
          vertices[ivertex].acceptorOcc.push_back(val);
          ++fillCount[ivertex];
          ++ivertex;
        }
        nAcceptor++;
	acceptorElectronXsec.push_back(-1);
	acceptorHoleXsec.push_back(-1);
	acceptorConc.push_back(-1);
      }
    }
  }
  if (datafile.fail() && !datafile.eof()) {
    std::cerr << m_className << "::LoadData\n";
    std::cerr << "    Error reading file " << datafilename << "\n";
    datafile.close();
    Cleanup();
    return false;
  }

  for (int i = nVertices; i--;) {
    if (fillCount[i] > 1) vertices[i].isShared = true;
  }

  datafile.close();

  return true;
}

bool ComponentTcad2d::LoadGrid(const std::string gridfilename) {

  // Open the file containing the mesh description.
  std::ifstream gridfile;
  gridfile.open(gridfilename.c_str(), std::ios::in);
  if (!gridfile) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not open file " << gridfilename << ".\n";
    return false;
  }

  std::string line;
  std::istringstream data;

  // Delete existing mesh information.
  Cleanup();
  std::string::size_type pBra, pKet, pEq;
  // Count line numbers.
  int iLine = 0;

  // Get the number of regions.
  while (!gridfile.fail()) {
    // Read one line.
    std::getline(gridfile, line);
    ++iLine;
    // Strip white space from the beginning of the line.
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'nb_regions'.
    if (line.substr(0, 10) == "nb_regions") {
      pEq = line.find('=');
      if (pEq == std::string::npos) {
        // No "=" sign found.
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of regions.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pEq + 1);
      data.str(line);
      data >> nRegions;
      data.clear();
      break;
    }
    if (gridfile.fail()) break;
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find entry 'nb_regions' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }
  regions.resize(nRegions);
  for (int j = nRegions; j--;) {
    regions[j].name = "";
    regions[j].drift = false;
    regions[j].medium = 0;
  }

  if (m_debug) {
    std::cout << m_className << "::LoadGrid:\n";
    std::cout << "    Found " << nRegions << " regions.\n";
  }

  // Get the region names.
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find entry 'regions'.
    if (line.substr(0, 7) == "regions") {
      // Get region names (given in brackets).
      pBra = line.find('[');
      pKet = line.find(']');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets [].
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read region names.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      for (int j = 0; j < nRegions; ++j) {
        data >> regions[j].name;
        data.clear();
        // Assume by default that all regions are active.
        regions[j].drift = true;
        regions[j].medium = 0;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    // Reached end of file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find entry 'regions' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    // Error reading from the file.
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Get the vertices.
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Vertices'.
    if (line.substr(0, 8) == "Vertices") {
      // Get number of vertices (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets [].
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of vertices.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> nVertices;
      data.clear();
      vertices.resize(nVertices);
      // Get the coordinates of this vertex.
      for (int j = 0; j < nVertices; ++j) {
        gridfile >> vertices[j].x >> vertices[j].y;
        // Change units from micron to cm.
        vertices[j].x *= 1.e-4;
        vertices[j].y *= 1.e-4;
        ++iLine;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Vertices' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Get the "edges" (lines connecting two vertices).
  int nEdges = 0;
  // Temporary arrays for storing edge points.
  std::vector<int> edgeP1;
  edgeP1.clear();
  std::vector<int> edgeP2;
  edgeP2.clear();
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Edges'.
    if (line.substr(0, 5) == "Edges") {
      // Get the number of edges (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets ()
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of edges.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> nEdges;
      data.clear();
      edgeP1.resize(nEdges);
      edgeP2.resize(nEdges);
      // Get the indices of the two endpoints.
      for (int j = 0; j < nEdges; ++j) {
        gridfile >> edgeP1[j] >> edgeP2[j];
        ++iLine;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Edges' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  for (int i = nEdges; i--;) {
    // Make sure the indices of the edge endpoints are not out of range.
    if (edgeP1[i] < 0 || edgeP1[i] >= nVertices || edgeP2[i] < 0 ||
        edgeP2[i] >= nVertices) {
      std::cerr << m_className << "::LoadGrid:\n";
      std::cerr << "    Vertex index of edge " << i << " out of range.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
    // Make sure the edge is non-degenerate.
    if (edgeP1[i] == edgeP2[i]) {
      std::cerr << m_className << "::LoadGrid:\n";
      std::cerr << "    Edge " << i << " is degenerate.\n";
      Cleanup();
      gridfile.close();
      return false;
    }
  }

  // Get the elements.
  int edge0, edge1, edge2, edge3;
  int type;
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    ++iLine;
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Elements'.
    if (line.substr(0, 8) == "Elements") {
      // Get number of elements (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets ().
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read number of elements.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> nElements;
      data.clear();
      // Resize array of elements.
      elements.resize(nElements);
      // Get type and constituting edges of each element.
      for (int j = 0; j < nElements; ++j) {
        for (int k = nMaxVertices; k--;) elements[j].vertex[k] = -1;
        elements[j].nNeighbours = 0;
        elements[j].neighbours.clear();
        ++iLine;
        gridfile >> type;
        switch (type) {
          case 1:
            // Line
            gridfile >> edge0 >> edge1;
            if (edge0 < 0) edge0 = -edge0 - 1;
            if (edge1 < 0) edge1 = -edge1 - 1;
            // Make sure the indices are not out of range.
            if (edge0 >= nEdges || edge1 >= nEdges) {
              std::cerr << m_className << "::LoadGrid:\n";
              std::cerr << "    Error reading file " << gridfilename
                        << " (line " << iLine << ").\n";
              std::cerr << "    Edge index out of range.\n";
              Cleanup();
              gridfile.close();
              return false;
            }
            // Get the vertices of this element.
            // Negative edge index means that the sequence of the two points
            // is supposed to be inverted.
            // The actual index is then given by "-index - 1".
            // Orientt the line such that the first point is on the left.
            if (vertices[edgeP1[edge0]].x > vertices[edgeP2[edge0]].x) {
              elements[j].vertex[0] = edgeP2[edge0];
              elements[j].vertex[1] = edgeP1[edge0];
            } else {
              elements[j].vertex[0] = edgeP1[edge0];
              elements[j].vertex[1] = edgeP2[edge0];
            }
            break;
          case 2:
            // Triangle
            gridfile >> edge0 >> edge1 >> edge2;
            // Make sure the indices are not out of range.
            if (edge0 < 0) edge0 = -edge0 - 1;
            if (edge1 < 0) edge1 = -edge1 - 1;
            if (edge2 < 0) edge2 = -edge2 - 1;
            if (edge0 >= nEdges || edge1 >= nEdges || edge2 >= nEdges) {
              std::cerr << m_className << "::LoadGrid:\n";
              std::cerr << "    Error reading file " << gridfilename
                        << " (line " << iLine << ").\n";
              std::cerr << "    Edge index out of range.\n";
              Cleanup();
              gridfile.close();
              return false;
            }
            elements[j].vertex[0] = edgeP1[edge0];
            elements[j].vertex[1] = edgeP2[edge0];
            if (edgeP1[edge1] != elements[j].vertex[0] &&
                edgeP1[edge1] != elements[j].vertex[1]) {
              elements[j].vertex[2] = edgeP1[edge1];
            } else {
              elements[j].vertex[2] = edgeP2[edge1];
            }
            // Rearrange vertices such that point 0 is on the left.
            while (vertices[elements[j].vertex[0]].x >
                       vertices[elements[j].vertex[1]].x ||
                   vertices[elements[j].vertex[0]].x >
                       vertices[elements[j].vertex[2]].x) {
              const int tmp = elements[j].vertex[0];
              elements[j].vertex[0] = elements[j].vertex[1];
              elements[j].vertex[1] = elements[j].vertex[2];
              elements[j].vertex[2] = tmp;
            }
            break;
          case 3:
            // Rectangle
            gridfile >> edge0 >> edge1 >> edge2 >> edge3;
            // Make sure the indices are not out of range.
            if (edge0 >= nEdges || -edge0 - 1 >= nEdges || edge1 >= nEdges ||
                -edge1 - 1 >= nEdges || edge2 >= nEdges ||
                -edge2 - 1 >= nEdges || edge3 >= nEdges ||
                -edge3 - 1 >= nEdges) {
              std::cerr << m_className << "::LoadGrid:\n";
              std::cerr << "    Error reading file " << gridfilename
                        << " (line " << iLine << ").\n";
              std::cerr << "    Edge index out of range.\n";
              Cleanup();
              gridfile.close();
              return false;
            }
            if (edge0 >= 0)
              elements[j].vertex[0] = edgeP1[edge0];
            else
              elements[j].vertex[0] = edgeP2[-edge0 - 1];
            if (edge1 >= 0)
              elements[j].vertex[1] = edgeP1[edge1];
            else
              elements[j].vertex[1] = edgeP2[-edge1 - 1];
            if (edge2 >= 0)
              elements[j].vertex[2] = edgeP1[edge2];
            else
              elements[j].vertex[2] = edgeP2[-edge2 - 1];
            if (edge3 >= 0)
              elements[j].vertex[3] = edgeP1[edge3];
            else
              elements[j].vertex[3] = edgeP2[-edge3 - 1];

            // Rearrange vertices such that point 0 is on the left.
            while (vertices[elements[j].vertex[0]].x >
                       vertices[elements[j].vertex[1]].x ||
                   vertices[elements[j].vertex[0]].x >
                       vertices[elements[j].vertex[2]].x ||
                   vertices[elements[j].vertex[0]].x >
                       vertices[elements[j].vertex[3]].x) {
              const int tmp = elements[j].vertex[0];
              elements[j].vertex[0] = elements[j].vertex[1];
              elements[j].vertex[1] = elements[j].vertex[2];
              elements[j].vertex[2] = elements[j].vertex[3];
              elements[j].vertex[3] = tmp;
            }
            break;
          default:
            // Other element types are not permitted for 2d grids.
            std::cerr << m_className << "::LoadGrid:\n"
                      << "    Error reading file " << gridfilename << " (line "
                      << iLine << ").\n";
            std::cerr << "    Invalid element type (" << type
                      << ") for 2d mesh.\n";
            Cleanup();
            gridfile.close();
            return false;
            break;
        }
        elements[j].type = type;
        elements[j].region = -1;
      }
      break;
    }
  }
  if (gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Could not find section 'Elements' in file\n";
    std::cerr << "    " << gridfilename << ".\n";
    Cleanup();
    gridfile.close();
    return false;
  } else if (gridfile.fail()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << " (line " << iLine
              << ").\n";
    Cleanup();
    gridfile.close();
    return false;
  }

  // Assign regions to elements.
  std::string name;
  while (!gridfile.fail()) {
    std::getline(gridfile, line);
    line.erase(line.begin(), find_if(line.begin(), line.end(),
                                     not1(std::ptr_fun<int, int>(isspace))));
    // Find section 'Region'.
    if (line.substr(0, 6) == "Region") {
      // Get region name (given in brackets).
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Could not read region name.\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      data.str(line);
      data >> name;
      data.clear();
      int index = -1;
      for (int j = 0; j < nRegions; ++j) {
        if (name == regions[j].name) {
          index = j;
          break;
        }
      }
      if (index == -1) {
        // Specified region name is not in the list.
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " << gridfilename << ".\n";
        std::cerr << "    Unknown region " << name << ".\n";
        continue;
      }
      std::getline(gridfile, line);
      std::getline(gridfile, line);
      pBra = line.find('(');
      pKet = line.find(')');
      if (pKet < pBra || pBra == std::string::npos ||
          pKet == std::string::npos) {
        // No closed brackets ().
        std::cerr << m_className << "::LoadGrid:\n";
        std::cerr << "    Error reading file " << gridfilename << ".\n";
        std::cerr << "    Could not read number of elements in region " << name
                  << ".\n";
        Cleanup();
        gridfile.close();
        return false;
      }
      line = line.substr(pBra + 1, pKet - pBra - 1);
      int nElementsRegion;
      int iElement;
      data.str(line);
      data >> nElementsRegion;
      data.clear();
      for (int j = 0; j < nElementsRegion; ++j) {
        gridfile >> iElement;
        elements[iElement].region = index;
      }
    }
  }

  gridfile.close();
  if (gridfile.fail() && !gridfile.eof()) {
    std::cerr << m_className << "::LoadGrid:\n";
    std::cerr << "    Error reading file " << gridfilename << ".\n";
    Cleanup();
    return false;
  }

  return true;
}

void ComponentTcad2d::FindNeighbours() {

  std::vector<std::vector<bool> > adjacent;
  adjacent.resize(nElements);
  for (int i = nElements; i--;) {
    adjacent[i].resize(nElements);
    for (int j = nElements; j--;) {
      adjacent[i][j] = false;
    }
  }

  for (int i = nElements; i--;) {
    for (int m = nMaxVertices; m--;) {
      if (elements[i].vertex[m] < 0) continue;
      for (int j = nElements; j--;) {
        if (i == j || adjacent[i][j]) continue;
        for (int n = nMaxVertices; n--;) {
          if (elements[i].vertex[m] == elements[j].vertex[n]) {
            adjacent[i][j] = true;
            break;
          }
        }
      }
    }
  }

  for (int i = nElements; i--;) {
    elements[i].nNeighbours = 0;
    elements[i].neighbours.clear();
    for (int j = nElements; j--;) {
      if (adjacent[i][j]) {
        elements[i].neighbours.push_back(j);
        elements[i].nNeighbours += 1;
      }
    }
  }
}

void ComponentTcad2d::Cleanup() {

  // Vertices
  vertices.clear();
  nVertices = 0;

  // Elements
  elements.clear();
  nElements = 0;

  // Regions
  regions.clear();
  nRegions = 0;
}

bool ComponentTcad2d::CheckRectangle(const double x, const double y,
                                     const int i) {

  if (y < vertices[elements[i].vertex[0]].y ||
      x > vertices[elements[i].vertex[3]].x ||
      y > vertices[elements[i].vertex[1]].y) {
    return false;
  }

  // Map (x, y) to local variables (u, v) in [-1, 1].
  const double u =
      (x - 0.5 * (vertices[elements[i].vertex[0]].x +
                  vertices[elements[i].vertex[3]].x)) /
      (vertices[elements[i].vertex[3]].x - vertices[elements[i].vertex[0]].x);
  const double v =
      (y - 0.5 * (vertices[elements[i].vertex[0]].y +
                  vertices[elements[i].vertex[1]].y)) /
      (vertices[elements[i].vertex[1]].y - vertices[elements[i].vertex[0]].y);
  // Compute weighting factors for each corner.
  w[0] = (0.5 - u) * (0.5 - v);
  w[1] = (0.5 - u) * (0.5 + v);
  w[2] = (0.5 + u) * (0.5 + v);
  w[3] = (0.5 + u) * (0.5 - v);

  return true;
}

bool ComponentTcad2d::CheckTriangle(const double x, const double y,
                                    const int i) {

  if (x > vertices[elements[i].vertex[1]].x &&
      x > vertices[elements[i].vertex[2]].x)
    return false;
  if (y < vertices[elements[i].vertex[0]].y &&
      y < vertices[elements[i].vertex[1]].y &&
      y < vertices[elements[i].vertex[2]].y)
    return false;
  if (y > vertices[elements[i].vertex[0]].y &&
      y > vertices[elements[i].vertex[1]].y &&
      y > vertices[elements[i].vertex[2]].y)
    return false;

  // Map (x, y) onto local variables (b, c) such that
  // P = A + b * (B - A) + c * (C - A)
  // A point P is inside the triangle ABC if b, c > 0 and b + c < 1;
  // b, c are also weighting factors for points B, C
  const double v1x =
      vertices[elements[i].vertex[1]].x - vertices[elements[i].vertex[0]].x;
  const double v1y =
      vertices[elements[i].vertex[1]].y - vertices[elements[i].vertex[0]].y;
  const double v2x =
      vertices[elements[i].vertex[2]].x - vertices[elements[i].vertex[0]].x;
  const double v2y =
      vertices[elements[i].vertex[2]].y - vertices[elements[i].vertex[0]].y;

  w[1] = ((x - vertices[elements[i].vertex[0]].x) * v2y -
          (y - vertices[elements[i].vertex[0]].y) * v2x) /
         (v1x * v2y - v1y * v2x);
  if (w[1] < 0. || w[1] > 1.) return false;

  w[2] = ((vertices[elements[i].vertex[0]].x - x) * v1y -
          (vertices[elements[i].vertex[0]].y - y) * v1x) /
         (v1x * v2y - v1y * v2x);
  if (w[2] < 0. || w[1] + w[2] > 1.) return false;

  // Weighting factor for point A
  w[0] = 1. - w[1] - w[2];

  return true;
}

bool ComponentTcad2d::CheckLine(const double x, const double y, const int i) {

  if (x > vertices[elements[i].vertex[1]].x) return false;
  if (y < vertices[elements[i].vertex[0]].y &&
      y < vertices[elements[i].vertex[1]].y)
    return false;
  if (y > vertices[elements[i].vertex[0]].y &&
      y > vertices[elements[i].vertex[1]].y)
    return false;
  const double v1x =
      vertices[elements[i].vertex[1]].x - vertices[elements[i].vertex[0]].x;
  const double v1y =
      vertices[elements[i].vertex[1]].y - vertices[elements[i].vertex[0]].y;
  const double tx = (x - vertices[elements[i].vertex[0]].x) / v1x;
  if (tx < 0. || tx > 1.) return false;
  const double ty = (y - vertices[elements[i].vertex[0]].y) / v1y;
  if (ty < 0. || ty > 1.) return false;
  if (tx == ty) {
    // Compute weighting factors for endpoints A, B
    w[0] = tx;
    w[1] = 1. - w[0];
    return true;
  }
  return false;
}

void ComponentTcad2d::Reset() {

  Cleanup();
  hasRangeZ = false;
  m_ready = false;
}

void ComponentTcad2d::UpdatePeriodicity() {

  if (!m_ready) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Field map not available.\n";
    return;
  }

  // Check for conflicts.
  if (m_xPeriodic && m_xMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along x requested; reset.\n";
    m_xPeriodic = m_xMirrorPeriodic = false;
  }

  if (m_yPeriodic && m_yMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Both simple and mirror periodicity\n";
    std::cerr << "    along y requested; reset.\n";
    m_yPeriodic = m_yMirrorPeriodic = false;
  }

  if (m_zPeriodic || m_zMirrorPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Periodicity along z requested; reset.\n";
    m_zPeriodic = m_zMirrorPeriodic = false;
  }

  if (m_xAxiallyPeriodic || m_yAxiallyPeriodic || m_zAxiallyPeriodic) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Axial symmetry is not supported; reset.\n";
    m_xAxiallyPeriodic = m_yAxiallyPeriodic = m_zAxiallyPeriodic = false;
  }

  if (m_xRotationSymmetry || m_yRotationSymmetry || m_zRotationSymmetry) {
    std::cerr << m_className << "::UpdatePeriodicity:\n";
    std::cerr << "    Rotation symmetry is not supported; reset.\n";
    m_xRotationSymmetry = m_yRotationSymmetry = m_zRotationSymmetry = false;
  }
}
}
