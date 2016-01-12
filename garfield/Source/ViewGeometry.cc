#include <iostream>
#include <cmath>

#include "GeometrySimple.hh"
#include "Solid.hh"
#include "Plotting.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "ViewGeometry.hh"

namespace Garfield {

ViewGeometry::ViewGeometry()
    : m_className("ViewGeometry"),
      m_debug(false),
      m_canvas(NULL),
      m_hasExternalCanvas(false),
      m_geometry(NULL),
      m_geoManager(NULL) {

  plottingEngine.SetDefaultStyle();
}

ViewGeometry::~ViewGeometry() {

  if (!m_hasExternalCanvas && m_canvas != NULL) delete m_canvas;
  Reset();
}

void ViewGeometry::SetGeometry(GeometrySimple* geo) {

  if (geo == NULL) {
    std::cerr << m_className << "::SetGeometry:\n";
    std::cerr << "    Geometry pointer is null.\n";
    return;
  }

  m_geometry = geo;
}

void ViewGeometry::SetCanvas(TCanvas* c) {

  if (c == NULL) return;
  if (!m_hasExternalCanvas && m_canvas != NULL) {
    delete m_canvas;
    m_canvas = NULL;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewGeometry::Plot() {

  if (m_geometry == NULL) {
    std::cerr << m_className << "::Plot:\n";
    std::cerr << "    Geometry is not defined.\n";
    return;
  }

  if (m_canvas == NULL) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle(m_label.c_str());
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();

  const unsigned int nSolids = m_geometry->GetNumberOfSolids();
  if (nSolids == 0) {
    std::cerr << m_className << "::Plot:\n";
    std::cerr << "    Geometry is empty.\n";
    return;
  }

  // Get the bounding box.
  double xMin = 0., yMin = 0., zMin = 0.;
  double xMax = 0., yMax = 0., zMax = 0.; 
  if (!m_geometry->GetBoundingBox(xMin, yMin, zMin, xMax, yMax, zMax)) {
    std::cerr << m_className << "::Plot:\n";
    std::cerr << "    Cannot retrieve bounding box.\n";
    return;
  }
  m_geoManager = new TGeoManager("ViewGeometryGeoManager", m_label.c_str());
  TGeoMaterial* matVacuum = new TGeoMaterial("Vacuum", 0., 0., 0.);
  TGeoMedium* medVacuum = new TGeoMedium("Vacuum", 1, matVacuum);
  m_media.push_back(medVacuum);
  // Use silicon as "default" material.
  TGeoMaterial* matDefault = new TGeoMaterial("Default", 28.085, 14., 2.329);
  TGeoMedium* medDefault = new TGeoMedium("Default", 1, matDefault); 
  TGeoVolume* world = m_geoManager->MakeBox("World", medVacuum,
                                            std::max(fabs(xMin), fabs(xMax)),
                                            std::max(fabs(yMin), fabs(yMax)),
                                            std::max(fabs(zMin), fabs(zMax)));
  m_geoManager->SetTopVolume(world);
  m_volumes.push_back(world);

  for (unsigned int i = 0; i < nSolids; ++i) {
    Solid* solid = m_geometry->GetSolid(i);
    if (solid == NULL) {
      std::cerr << m_className << "::Plot:\n";
      std::cerr << "    Could not get solid " << i << " from geometry.\n";
      continue;
    }
    // Get the center coordinates.
    double x0 = 0., y0 = 0., z0 = 0.;
    if (!solid->GetCenter(x0, y0, z0)) {
      std::cerr << m_className << "::Plot:\n";
      std::cerr << "    Could not determine solid center.\n";
      continue;
    }
    // Get the rotation.
    double ctheta = 1., stheta = 0.;
    double cphi = 1., sphi = 0.;
    if (!solid->GetOrientation(ctheta, stheta, cphi, sphi)) {
      std::cerr << m_className << "::Plot:\n";
      std::cerr << "    Could not determine solid orientation.\n";
      continue;
    }
    double matrix[9] = {cphi * ctheta, -sphi, cphi * stheta,
                        sphi * ctheta,  cphi, sphi * stheta,
                              -stheta,     0,        ctheta};
    TGeoVolume* volume = NULL;
    if (solid->IsTube()) {
      double rmin = 0., rmax = 0., lz = 0.;
      if (!solid->GetDimensions(rmin, rmax, lz)) {
        std::cerr << m_className << "::Plot:\n";
        std::cerr << "    Could not determine tube dimensions.\n";
        continue;
      }
      volume = m_geoManager->MakeTube("Tube", medDefault, rmin, rmax, lz);
    } else if (solid->IsBox()) {
      double dx = 0., dy = 0., dz = 0.;
      if (!solid->GetDimensions(dx, dy, dz)) {
        std::cerr << m_className << "::Plot:\n";
        std::cerr << "    Could not determine box dimensions.\n";
        continue;
      }
      volume = m_geoManager->MakeBox("Box", medDefault, dx, dy, dz);
    } else if (solid->IsSphere()) {
      double rmin = 0., rmax = 0., dummy = 0.;
      if (!solid->GetDimensions(rmin, rmax, dummy)) {
        std::cerr << m_className << "::Plot:\n";
        std::cerr << "    Could not determine sphere dimensions.\n";
        continue;
      }
      volume = m_geoManager->MakeSphere("Sphere", medDefault, rmin, rmax); 
    } else {
      std::cerr << m_className << "::Plot:\n";
      std::cerr << "    Unknown solid type.\n";
      continue;
    }
    Medium* medium = m_geometry->GetMedium(x0, y0, z0);
    if (medium == NULL) {
      volume->SetLineColor(kGreen + 2);
      volume->SetTransparency(50);
    } else if (medium->IsGas()) {
      volume->SetLineColor(kBlue + medium->GetId());
      volume->SetTransparency(50);
    } else if (medium->IsSemiconductor()) {
      volume->SetLineColor(kRed + medium->GetId());
      volume->SetTransparency(50);
    } else {
      volume->SetLineColor(kViolet + medium->GetId());
      volume->SetTransparency(0);
    } 
    TGeoRotation r;
    r.SetMatrix(matrix);
    TGeoTranslation t(x0, y0, z0);
    TGeoCombiTrans* transform = new TGeoCombiTrans(t, r);
    m_volumes.push_back(volume);
    m_geoManager->GetTopVolume()->AddNode(volume, 1, transform);
  }
  m_geoManager->CloseGeometry();
  m_geoManager->GetTopNode()->Draw("ogl");

}

void ViewGeometry::Reset() {

 for (std::vector<TGeoVolume*>::iterator it = m_volumes.begin();
      it != m_volumes.end(); ++it) {
    if (*it) {
      TGeoShape* shape = (*it)->GetShape();
      if (shape) delete shape;
      delete *it;
    }
  }
  m_volumes.clear();
  for (std::vector<TGeoMedium*>::iterator it = m_media.begin();
       it != m_media.end(); ++it) {
    if (*it) {
      TGeoMaterial* material = (*it)->GetMaterial();
      if (material) delete material;
      delete *it;
    }
  }
  m_media.clear();

  if (m_geoManager) {
    delete m_geoManager;
    m_geoManager = NULL;
  }

}

}
