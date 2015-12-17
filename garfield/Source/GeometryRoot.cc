#include <iostream>
#include <cmath>

#include <TGeoNode.h>
#include <TGeoBBox.h>
#include <TList.h>

#include "GeometryRoot.hh"

namespace Garfield {

GeometryRoot::GeometryRoot() 
    : m_geoManager(NULL), m_nMaterials(0), m_debug(false) {

  m_className = "GeometryRoot";
}

void GeometryRoot::SetGeometry(TGeoManager* geoman) {

  if (geoman == NULL) {
    std::cerr << "GeometryRoot::SetGeometry:\n";
    std::cerr << "    Pointer to TGeoManager is null.\n";
    return;
  }

  m_geoManager = geoman;
  m_materials.clear();
  m_nMaterials = 0;
}

Medium* GeometryRoot::GetMedium(const double x, const double y, 
                                const double z) const {

  if (!m_geoManager) return NULL; 
  m_geoManager->SetCurrentPoint(x, y, z);
  if (m_geoManager->IsOutside()) return NULL;
  TGeoNode* cnode = m_geoManager->GetCurrentNode();
  std::string name(cnode->GetMedium()->GetMaterial()->GetName());

  for (int i = m_nMaterials; i--;) {
    if (m_materials[i].name == name) {
      return m_materials[i].medium;
    }
  }
  return NULL;
}

int GeometryRoot::GetNumberOfMaterials() {

  if (!m_geoManager) {
    std::cerr << "GeometryRoot::GetNumberOfMaterials:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return 0;
  }

  return m_geoManager->GetListOfMaterials()->GetEntries();
}

TGeoMaterial* GeometryRoot::GetMaterial(const int i) {

  if (!m_geoManager) {
    std::cerr << "GeometryRoot::GetMaterial:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return NULL;
  }

  return m_geoManager->GetMaterial(i);
}

TGeoMaterial* GeometryRoot::GetMaterial(const char* name) {

  if (!m_geoManager) {
    std::cerr << "GeometryRoot::GetMaterial:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return NULL;
  }

  return m_geoManager->GetMaterial(name);
}

void GeometryRoot::SetMedium(const int imat, Medium* med) {

  if (!m_geoManager) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return;
  }

  if (!med) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  TGeoMaterial* mat = m_geoManager->GetMaterial(imat);
  if (!mat) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    ROOT material with index " << imat
              << " does not exist.\n";
    return;
  }

  std::string name(mat->GetName());
  bool isNew = true;
  // Check if this material has already been associated with a medium
  for (int i = m_nMaterials; i--;) {
    if (name == m_materials[i].name) {
      std::cout << "GeometryRoot::SetMedium:\n";
      std::cout << "    Current association of material " << name
                << " with medium " << med->GetName() << " is overwritten.\n";
      m_materials[i].medium = med;
      isNew = false;
      break;
    }
  }

  if (isNew) {
    material newMaterial;
    newMaterial.name = name;
    newMaterial.medium = med;
    m_materials.push_back(newMaterial);
    ++m_nMaterials;
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

void GeometryRoot::SetMedium(const char* name, Medium* med) {

  if (!m_geoManager) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    ROOT geometry is not defined.\n";
    std::cerr << "    Call SetGeometry first.\n";
    return;
  }

  if (!med) {
    std::cerr << "GeometryRoot::SetMedium:\n";
    std::cerr << "    Medium pointer is null.\n";
    return;
  }

  const int imat = m_geoManager->GetMaterialIndex(name);
  if (imat < 0) {
    std::cerr << "GeometryRoot::SetMedium:" << std::endl;
    std::cerr << "    ROOT material " << name << " does not exist."
              << std::endl;
    return;
  }

  SetMedium(imat, med);
}

bool GeometryRoot::GetBoundingBox(double& xmin, double& ymin, double& zmin,
                                  double& xmax, double& ymax, double& zmax) {

  if (!m_geoManager) return false;
  TGeoBBox* box = (TGeoBBox*)m_geoManager->GetTopVolume()->GetShape();
  const double dx = box->GetDX();
  const double dy = box->GetDY();
  const double dz = box->GetDZ();
  const double ox = box->GetOrigin()[0];
  const double oy = box->GetOrigin()[1];
  const double oz = box->GetOrigin()[2];
  xmin = ox - dx;
  xmax = ox + dx;
  ymin = oy - dy;
  ymax = oy + dy;
  zmin = oz - dz;
  zmax = oz + dz;
  return true;
}
}
