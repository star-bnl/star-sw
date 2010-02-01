#include <iostream>
#include "ComponentBase.hh"

namespace Garfield {

ComponentBase::ComponentBase() :
  nMedia(0), nSolids(0),
  hasBoundingBox(false),
  ready(false), 
  xPeriodic(false),         yPeriodic(false),         zPeriodic(false),
  xMirrorPeriodic(false),   yMirrorPeriodic(false),   zMirrorPeriodic(false),
  xAxiallyPeriodic(false),  yAxiallyPeriodic(false),  zAxiallyPeriodic(false),
  xRotationSymmetry(false), yRotationSymmetry(false), zRotationSymmetry(false),
  bx0(0.), by0(0.), bz0(0.),
  debug(false), warning(false) {

  media.clear();
  solids.clear();

}

void 
ComponentBase::AddSolid(Solid* s, Medium* m, int bctype, double bcval) {

  // Make sure the solid is defined
  if (s == 0) {
    std::cerr << "ComponentBase::AddSolid:" << std::endl;
    std::cerr << "    Solid is not defined." << std::endl;
    return;
  }
 
  // Make sure the component can deal with this type of solid
  if (!CheckSolidType(s)) {
    std::cerr << "ComponentBase::AddSolid:" << std::endl;
    std::cerr << "    Cannot deal with this type of solid." << std::endl;
    return;
  }

  int n = -1;
  if (m != 0) {
    int id = m->GetId();
    // Check if this medium is already in the list
    for (int i = nMedia; i--;) {      
      if (id == media[i].medium->GetId()) {
        n = i;
        break;
      }
    }
    // If the medium does not exist yet, add it to the list
    if (n < 0) {
      medium newMedium;
      newMedium.medium = m;
      media.push_back(newMedium);
      n = nMedia;
      ++nMedia;      
    }
  }
  
  // Check the validity of the specified boundary condition
  // and modify it if necessary
  CheckBoundaryConditionType(bctype, bcval);
  
  // Update the bounding box ranges
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!s->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << "ComponentBase::AddSolid:" << std::endl;
    std::cerr << "    Solid has no bounding box." << std::endl;
    return;
  }
  
  if (hasBoundingBox) {
    if (xmin < xMinBoundingBox) xMinBoundingBox = xmin;
    if (ymin < yMinBoundingBox) yMinBoundingBox = ymin;
    if (zmin < zMinBoundingBox) zMinBoundingBox = zmin;
    if (xmax > xMaxBoundingBox) xMaxBoundingBox = xmax;
    if (ymax > yMaxBoundingBox) yMaxBoundingBox = ymax;
    if (zmax > zMaxBoundingBox) zMaxBoundingBox = zmax;
  } else {
    xMinBoundingBox = xmin; yMinBoundingBox = ymin; zMinBoundingBox = zmin;
    xMaxBoundingBox = xmax; yMaxBoundingBox = ymax; zMaxBoundingBox = zmax;
    hasBoundingBox = true;
  }  

  // Add the new solid to the list
  solid newSolid;
  newSolid.solid = s;
  newSolid.medium = n;
  newSolid.bctype = bctype;
  newSolid.bcval = bcval;
  solids.push_back(newSolid);
  ++nSolids;    

}

bool 
ComponentBase::GetSolid(const double x, const double y, const double z, 
                        Solid*& s) {
                             
  for (int i = nSolids; i--;) {
    if (solids[i].solid->IsInside(x, y, z)) {
      s = solids[i].solid;
      return true;
    }
  }
  return false;
  
}

bool 
ComponentBase::GetMedium(const double x, const double y, const double z, 
                         Medium*& m) {
               
  for (int i = nSolids; i--;) {
    if (solids[i].solid->IsInside(x, y, z)) {
      if (solids[i].medium < 0) return false;
      m = media[solids[i].medium].medium;
      return true;
    }
  }
  return false;
               
}

bool 
ComponentBase::GetSolid(const int i, Solid*& s) const {

  if (i < 0 || i >= nSolids) {
    std::cerr << "ComponentBase::GetSolid:" << std::endl;
    std::cerr << "    Requested solid " << i << " does not exist." << std::endl;
    return false;
  }
  
  s = solids[i].solid;  
  return true;

}

bool 
ComponentBase::GetMedium(const int i, Medium*& m) const {

  if (i < 0 || i >= nMedia) {
    std::cerr << "ComponentBase::GetMedium:" << std::endl;
    std::cerr << "    Requested medium " << i << " does not exist." << std::endl;
    return false;
  }
  
  m = media[i].medium;
  return true;

}

void 
ComponentBase::Clear() {

  media.clear();
  solids.clear();
  nMedia = 0;
  nSolids = 0;
  
  Reset();

}


void 
ComponentBase::WeightingField(const double x, const double y, const double z,
                              double& wx, double& wy, double& wz,
                              const std::string label) {
                              
  std::cerr << "ComponentBase: WeightingField:" << std::endl;
  std::cerr << "    This function is not implemented." << std::endl;
  wx = wy = wz = 0.;
  
}

void 
ComponentBase::MagneticField(const double x, const double y, const double z,
    	                     double& bx, double& by, double& bz, int& status) {

  bx = bx0; by = by0; bz = bz0;
  status = 0;

}

void 
ComponentBase::SetMagneticField(const double bx, const double by, const double bz) {

  bx0 = bx; by0 = by; bz0 = bz;

}

bool
ComponentBase::IsInBoundingBox(const double x, const double y, const double z) {

  if (!hasBoundingBox) {
    if (debug) {
      std::cerr << "ComponentBase::IsInBoundingBox:" << std::endl;
      std::cerr << "    Bounding box is not defined." << std::endl;
    }
    return true;
  }
  
  if (x >= xMinBoundingBox && x <= xMaxBoundingBox && 
      y >= yMinBoundingBox && y <= yMaxBoundingBox &&
      z >= zMinBoundingBox && z <= zMaxBoundingBox) return true;
  return false;

}

void 
ComponentBase::EnableDebugging() {

  debug = true;
  
}

void 
ComponentBase::DisableDebugging() {

  debug = false;
  
}

void 
ComponentBase::EnableWarnings() {

  warning = true;
  
}

void 
ComponentBase::DisableWarnings() {

  warning = false;
  
}

}
