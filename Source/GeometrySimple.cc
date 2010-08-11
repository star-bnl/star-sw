#include <iostream>
#include "GeometrySimple.hh"

namespace Garfield {

GeometrySimple::GeometrySimple() :
  nMedia(0), nSolids(0),
  hasBoundingBox(false),
  debug(false) {

  media.clear();
  solids.clear();

}

void 
GeometrySimple::AddSolid(Solid* s, Medium* m) {

  // Make sure the solid and the medium are defined.
  if (s == 0) {
    std::cerr << "GeometrySimple::AddSolid:\n";
    std::cerr << "    Solid pointer is null.\n";
    return;
  }

  if (m == 0) {
    std::cerr << "GeometrySimple::AddSolid:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }
 
  int n = -1;
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
  
  // Update the bounding box ranges
  double xmin, ymin, zmin;
  double xmax, ymax, zmax;
  if (!s->GetBoundingBox(xmin, ymin, zmin, xmax, ymax, zmax)) {
    std::cerr << "GeometrySimple::AddSolid:\n";
    std::cerr << "    Solid has no bounding box.\n";
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
  solids.push_back(newSolid);
  ++nSolids;    

}

bool 
GeometrySimple::GetSolid(const double x, const double y, const double z, 
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
GeometrySimple::GetMedium(const double x, const double y, const double z, 
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
GeometrySimple::GetSolid(const int i, Solid*& s) const {

  if (i < 0 || i >= nSolids) {
    std::cerr << "GeometrySimple::GetSolid:\n";
    std::cerr << "    Requested solid " << i << " does not exist.\n";
    return false;
  }
  
  s = solids[i].solid;  
  return true;

}

bool 
GeometrySimple::GetMedium(const int i, Medium*& m) const {

  if (i < 0 || i >= nMedia) {
    std::cerr << "GeometrySimple::GetMedium:\n";
    std::cerr << "    Requested medium " << i << " does not exist.\n";
    return false;
  }
  
  m = media[i].medium;
  return true;

}

void 
GeometrySimple::Clear() {

  media.clear();
  solids.clear();
  nMedia = 0;
  nSolids = 0;

}

bool
GeometrySimple::IsInside(const double x, const double y, const double z) {

  if (!IsInBoundingBox(x, y, z)) return false;
  
  for (int i = nSolids; i--;) {
    if (solids[i].solid->IsInside(x, y, z)) return true;
  }
  return false;

}

bool
GeometrySimple::IsInBoundingBox(
        const double x, const double y, const double z) {

  if (!hasBoundingBox) {
    if (debug) {
      std::cerr << "GeometrySimple::IsInBoundingBox:\n";
      std::cerr << "    Bounding box is not defined.\n";
    }
    return true;
  }
  
  if (x >= xMinBoundingBox && x <= xMaxBoundingBox && 
      y >= yMinBoundingBox && y <= yMaxBoundingBox &&
      z >= zMinBoundingBox && z <= zMaxBoundingBox) return true;
  return false;

}

}
