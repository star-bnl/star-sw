#include <iostream>
#include <cmath>

#include <TGeoNode.h>
#include <TGeoBBox.h>
#include <TList.h>

#include "GeometryRoot.hh"

namespace Garfield {

GeometryRoot::GeometryRoot() :
  theGeoManager(0),
  nMaterials(0), 
  debug(false) {

  materials.clear();

}

void
GeometryRoot::SetGeometry(TGeoManager* geoman) {

  if (geoman == 0) {
    std::cerr << "GeometryRoot::SetGeometry:\n";
    std::cerr << "    Pointer to TGeoManager is null.\n";
    return;
  }

  theGeoManager = geoman;
  materials.clear();
  nMaterials = 0;

}

bool 
GeometryRoot::GetMedium(const double x, const double y, const double z, 
                        Medium*& m) {

  m = 0;  
  if (theGeoManager == 0) return false;
  theGeoManager->SetCurrentPoint(x, y, z);
  if (theGeoManager->IsOutside()) return false;
  TGeoNode* cnode = theGeoManager->GetCurrentNode();
  std::string name(cnode->GetMedium()->GetMaterial()->GetName());

  for (int i = nMaterials; i--;) {
    if (materials[i].name == name) {
      m = materials[i].medium;
      return true;
    }
  }
  return false;
               
}

int
GeometryRoot::GetNumberOfMaterials() {

  if (theGeoManager == 0) {
    std::cerr << "GeometryRoot::GetNumberOfMaterials:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return 0;
  }
  
  return theGeoManager->GetListOfMaterials()->GetEntries(); 

}

TGeoMaterial*
GeometryRoot::GetMaterial(const int i) {

  if (theGeoManager == 0) {
    std::cerr << "GeometryRoot::GetMaterial:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return 0;
  }

  return theGeoManager->GetMaterial(i);

}


TGeoMaterial*
GeometryRoot::GetMaterial(const char* name) {

  if (theGeoManager == 0) {
    std::cerr << "GeometryRoot::GetMaterial:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return 0;
  }

  return theGeoManager->GetMaterial(name);

}

void
GeometryRoot::SetMedium(const int imat, Medium* med) {

  if (theGeoManager == 0) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return;
  }

  if (med == 0) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  TGeoMaterial* mat = theGeoManager->GetMaterial(imat);
  if (mat == 0) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    ROOT material with index " << imat 
              << " does not exist.\n";
    return;
  }

  std::string name(mat->GetName());
  bool isNew = true;
  // Check if this material has already been associated with a medium
  for (int i = nMaterials; i--;) {
    if (name == materials[i].name) {
      std::cout << "GeometryRoot::SetMedium:\n";
      std::cout << "    Current association of material " << name 
                << " with medium " << med->GetName() 
                << " is overwritten.\n";
      materials[i].medium = med;
      isNew = false;
      break;
    } 
  }

  if (isNew) {
    material newMaterial;
    newMaterial.name = name;
    newMaterial.medium = med;
    materials.push_back(newMaterial);
    ++nMaterials;
  }

  // Check if material properties match
  const double rho1 = mat->GetDensity();
  const double rho2 = med->GetMassDensity();
  std::cout << "GeometryROOT::SetMedium:\n";
  std::cout << "    ROOT material: " << name << "\n";
  std::cout << "      Density: " << rho1 << " g / cm3\n";
  std::cout << "    Medium: " << med->GetName() << "\n";
  std::cout << "      Density: " << rho2 << " g / cm3\n";
  if (rho1 > 0 && fabs(rho1 - rho2) / rho1 > 0.01) {
    std::cout << "    WARNING: Densities differ by > 1%.\n";
  }

}

void
GeometryRoot::SetMedium(const char* name, Medium* med) {

  if (theGeoManager == 0) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return;
  }

  if (med == 0) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  int imat = theGeoManager->GetMaterialIndex(name);
  if (imat < 0) {
    std::cerr << "GeometryRoot::SetMedium:" << std::endl;
    std::cerr << "    ROOT material " << name 
              << " does not exist." << std::endl;
    return;
  }

  SetMedium(imat, med);

}

bool
GeometryRoot::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                             double& xmax, double& ymax, double& zmax) {

  if (theGeoManager == 0) return false;
  TGeoBBox* box = (TGeoBBox*)theGeoManager->GetTopVolume()->GetShape();
  const double dx = box->GetDX();
  const double dy = box->GetDY();
  const double dz = box->GetDZ();
  const double ox = box->GetOrigin()[0];
  const double oy = box->GetOrigin()[1];
  const double oz = box->GetOrigin()[2];
  xmin = ox - dx; xmax = ox + dx;
  ymin = oy - dy; ymax = oy + dy;
  zmin = oz - dz; zmax = oz + dz;
  return true;

}

}
