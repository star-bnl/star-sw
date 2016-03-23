#include <iostream>
#include <sstream>
#include <stdio.h>
#include <string.h>
#include <cmath>

#include <TROOT.h>
#include <TAxis.h>

#include "Plotting.hh"
#include "Sensor.hh"
#include "ComponentBase.hh"
#include "ViewField.hh"

namespace Garfield {

ViewField::ViewField()
    : m_className("ViewField"),
      m_debug(false),
      m_useStatus(false),
      m_vBkg(0.),
      m_sensor(NULL),
      m_component(NULL),
      m_pxmin(-1.),
      m_pymin(-1.),
      m_pxmax(1.),
      m_pymax(1.),
      m_fmin(0.),
      m_fmax(100.),
      m_emin(0.),
      m_emax(10000.),
      m_wmin(0.),
      m_wmax(100.),
      m_nContours(nMaxContours),
      m_nSamples1d(1000),
      m_nSamples2dX(200),
      m_nSamples2dY(200),
      m_electrode(""),
      m_canvas(NULL),
      m_hasExternalCanvas(false),
      m_fPot(NULL),
      m_fWfield(NULL),
      m_fPotProfile(NULL) {

  SetDefaultProjection();
  plottingEngine.SetDefaultStyle();
}

ViewField::~ViewField() {

  if (!m_hasExternalCanvas && m_canvas) delete m_canvas;
  if (m_fPot) delete m_fPot;
  if (m_fWfield) delete m_fWfield;
  if (m_fPotProfile) delete m_fPotProfile;
}

void ViewField::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  m_sensor = s;
  m_component = NULL;
  // Get the bounding box.
  bool ok = m_sensor->GetArea(m_pxmin, m_pymin, m_pzmin, m_pxmax, m_pymax, m_pzmax);
  if (!ok) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Warning: bounding box of sensor is not defined.\n";
  }
  // Get the voltage range.
  ok = m_sensor->GetVoltageRange(m_fmin, m_fmax);
  if (!ok) {
    std::cerr << m_className << "::SetSensor:\n";
    std::cerr << "    Warning: voltage range of sensor is not defined.\n";
  }
}

void ViewField::SetComponent(ComponentBase* c) {

  if (!c) {
    std::cerr << m_className << "::SetComponent:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  m_component = c;
  m_sensor = NULL;
  // Get the bounding box.
  bool ok = m_component->GetBoundingBox(m_pxmin, m_pymin, m_pzmin, 
                                        m_pxmax, m_pymax, m_pzmax);
  if (!ok) {
    std::cerr << m_className << "::SetComponent:\n";
    std::cerr << "    Warning: bounding box of component is not defined.\n";
  }
  // Get the voltage range.
  ok = m_component->GetVoltageRange(m_fmin, m_fmax);
  if (!ok) {
    std::cerr << m_className << "::SetComponent:\n";
    std::cerr << "    Warning: voltage range of component is not defined.\n";
  }
}

void ViewField::SetCanvas(TCanvas* c) {

  if (!c) return;
  if (!m_hasExternalCanvas && m_canvas) {
    delete m_canvas;
    m_canvas = NULL;
  }
  m_canvas = c;
  m_hasExternalCanvas = true;
}

void ViewField::SetArea(const double xmin, const double ymin, 
                        const double xmax, const double ymax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax) {
    std::cerr << m_className << "::SetArea:\n";
    std::cerr << "    Null area range not permitted.\n";
    std::cerr << "      " << xmin << " < x < " << xmax << "\n";
    std::cerr << "      " << ymin << " < y < " << ymax << "\n";
    return;
  }
  m_pxmin = std::min(xmin, xmax);
  m_pymin = std::min(ymin, ymax);
  m_pxmax = std::max(xmin, xmax);
  m_pymax = std::max(ymin, ymax);
}

void ViewField::SetVoltageRange(const double minval, const double maxval) {

  m_fmin = std::min(minval, maxval);
  m_fmax = std::max(minval, maxval);
}

void ViewField::SetElectricFieldRange(const double minval,
                                      const double maxval) {

  m_emin = std::min(minval, maxval);
  m_emax = std::max(minval, maxval);
}

void ViewField::SetWeightingFieldRange(const double minval,
                                       const double maxval) {

  m_wmin = std::min(minval, maxval);
  m_wmax = std::max(minval, maxval);
}

void ViewField::SetNumberOfContours(const unsigned int n) {

  if (n <= nMaxContours) {
    m_nContours = n;
  } else {
    std::cerr << m_className << "::SetNumberOfContours:\n";
    std::cerr << "    Max. number of contours is " << nMaxContours << ".\n";
  }
}

void ViewField::SetNumberOfSamples1d(const unsigned int n) {

  const unsigned int nmin = 10;
  const unsigned int nmax = 100000;

  if (n < nmin || n > nmax) {
    std::cerr << m_className << "::SetNumberOfSamples1d:\n";
    std::cerr << "    Number of points (" << n << ") out of range.\n";
    std::cerr << "    " << nmin << " <= n <= " << nmax << "\n";
    return;
  }

  m_nSamples1d = n;
}

void ViewField::SetNumberOfSamples2d(const unsigned int nx, 
                                     const unsigned int ny) {

  const unsigned int nmin = 10;
  const unsigned int nmax = 10000;
  if (nx < nmin || nx > nmax) {
    std::cerr << m_className << "::SetNumberOfSamples2d:\n";
    std::cerr << "    Number of x-points (" << nx << ") out of range.\n";
    std::cerr << "    " << nmin << " <= nx <= " << nmax << "\n";
  } else {
    m_nSamples2dX = nx;
  }

  if (ny < nmin || ny > nmax) {
    std::cerr << m_className << "::SetNumberOfSamples2d:\n";
    std::cerr << "    Number of y-points (" << ny << ") out of range.\n";
    std::cerr << "    " << nmin << " <= ny <= " << nmax << "\n";
  } else {
    m_nSamples2dY = ny;
  }
}

void ViewField::PlotContour(const std::string& option) {

  SetupCanvas(true);
  if (!m_fPot) CreateFunction();
  const int plotType = SetupFunction(option, m_fPot);

  double level[nMaxContours];
  const double ymin = plotType == 0 ? m_fmin : m_emin;
  const double ymax = plotType == 0 ? m_fmax : m_emax;
  for (unsigned int i = 0; i < m_nContours; ++i) {
    if (m_nContours > 1) {
      level[i] = ymin + i * (ymax - ymin) / (m_nContours - 1.);
    } else {
      level[i] = 0.5 * (ymax + ymin);
    }
  }
  m_fPot->SetContour(m_nContours, level);

  if (m_debug) {
    std::cout << m_className << "::PlotContour:\n";
    std::cout << "    Number of contours: " << m_nContours << "\n";
    for (unsigned int i = 0; i < m_nContours; ++i) {
      std::cout << "        Level " << i << " = " << level[i] << "\n";
    }
  }
  if (plotType == 0) {
    m_fPot->SetTitle("Contours of the potential");
  } else if (plotType == 1) {
    m_fPot->SetTitle("Contours of the electric field");
  } else if (plotType == 2) {
    m_fPot->SetTitle("Contours of the electric field (x-component)");
  } else if (plotType == 3) {
    m_fPot->SetTitle("Contours of the electric field (y-component)");
  } else if (plotType == 4) {
    m_fPot->SetTitle("Contours of the electric field (z-component)");
  }
  m_fPot->Draw("CONT4Z");
  m_canvas->Update();
}

void ViewField::PlotSurface(const std::string& option) {

  Plot(option, "SURF4");
}

void ViewField::Plot(const std::string& option, 
                     const std::string& drawopt) {

  SetupCanvas(true);
  if (!m_fPot) CreateFunction();
  const int plotType = SetupFunction(option, m_fPot);
  if (plotType == 0) {
    m_fPot->SetTitle("Potential");
  } else if (plotType == 1) {
    m_fPot->SetTitle("Electric field");
  } else if (plotType == 2) {
    m_fPot->SetTitle("Electric field (x-component)");
  } else if (plotType == 3) {
    m_fPot->SetTitle("Electric field (y-component)");
  } else if (plotType == 4) {
    m_fPot->SetTitle("Electric field (z-component)");
  }
  m_fPot->Draw(drawopt.c_str());
  m_canvas->Update();
}

void ViewField::PlotProfile(const double x0, const double y0, const double z0,
                            const double x1, const double y1, const double z1,
                            const std::string& option) {

  // Check the distance between the two points.
  const double d = sqrt(pow(x1 - x0, 2) + pow(y1 - y0, 2) + pow(z1 - z0, 2));
  if (d <= 0.) {
    std::cerr << m_className << "::PlotProfile:\n";
    std::cerr << "    Start and end point coincide.\n";
    return;
  }

  SetupCanvas(false);
  if (!m_fPotProfile) CreateProfileFunction();

  m_fPotProfile->SetParameter(0, x0);
  m_fPotProfile->SetParameter(1, y0);
  m_fPotProfile->SetParameter(2, z0);
  m_fPotProfile->SetParameter(3, x1);
  m_fPotProfile->SetParameter(4, y1);
  m_fPotProfile->SetParameter(5, z1);
  int plotType = 0;
  if (option == "v" || option == "p" || option == "phi" || option == "volt" ||
      option == "voltage" || option == "pot" || option == "potential") {
    m_fPotProfile->SetParameter(6, -1.);
    m_fPotProfile->SetMinimum(m_fmin);
    m_fPotProfile->SetMaximum(m_fmax);
  } else if (option == "e" || option == "field") {
    m_fPotProfile->SetParameter(6, 1.);
    plotType = 1;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else if (option == "ex") {
    m_fPotProfile->SetParameter(6, 11.);
    plotType = 2;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else if (option == "ey") {
    m_fPotProfile->SetParameter(6, 21.);
    plotType = 3;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else if (option == "ez") {
    m_fPotProfile->SetParameter(6, 31.);
    plotType = 4;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else {
    std::cerr << m_className << "::PlotProfile:\n";
    std::cerr << "    Unknown option (" << option << ")\n";
    std::cerr << "    Plotting the potential.\n";
    m_fPotProfile->SetParameter(6, -1.);
    m_fPotProfile->SetMinimum(m_fmin);
    m_fPotProfile->SetMaximum(m_fmax);
  }
  if (m_debug) {
    std::cout << m_className << "::PlotProfile:\n";
    if (plotType == 0) {
      std::cout << "    Plotting potential along\n";
    } else {
      std::cout << "    Plotting field along\n";
    }
    std::cout << "    (" << m_fPotProfile->GetParameter(0) << ", "
              << m_fPotProfile->GetParameter(1) << ", "
              << m_fPotProfile->GetParameter(2) << ") - ("
              << m_fPotProfile->GetParameter(3) << ", "
              << m_fPotProfile->GetParameter(4) << ", "
              << m_fPotProfile->GetParameter(5) << ")\n";
  }
  m_fPotProfile->GetXaxis()->SetTitle("normalised distance");
  if (plotType == 0) {
    m_fPotProfile->SetTitle("Profile plot of the potential");
    m_fPotProfile->GetYaxis()->SetTitle("potential [V]");
  } else if (plotType == 1) {
    m_fPotProfile->SetTitle("Profile plot of the electric field");
    m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
  } else if (plotType == 2) {
    m_fPotProfile->SetTitle("Profile plot of the electric field (x-component)");
    m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
  } else if (plotType == 3) {
    m_fPotProfile->SetTitle("Profile plot of the electric field (y-component)");
    m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
  } else if (plotType == 4) {
    m_fPotProfile->SetTitle("Profile plot of the electric field (z-component)");
    m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
  }
  m_fPotProfile->SetNpx(m_nSamples1d);
  m_fPotProfile->Draw();
  m_canvas->Update();
}

void ViewField::PlotSurfaceWeightingField(const std::string& label,
                                          const std::string& option) {

  m_electrode = label;

  SetupCanvas(true);
  if (!m_fWfield) CreateFunctionWeightingField();
  const int plotType = SetupFunction(option, m_fWfield);

  if (plotType == 0) {
    m_fWfield->SetTitle("Surface plot of the weighting potential");
  } else if (plotType == 1) {
    m_fWfield->SetTitle("Surface plot of the weighting field");
  } else if (plotType == 2) {
    m_fWfield->SetTitle("Surface plot of the weighting field (x-component)");
  } else if (plotType == 3) {
    m_fWfield->SetTitle("Surface plot of the weighting field (y-component)");
  } else if (plotType == 4) {
    m_fWfield->SetTitle("Surface plot of the weighting field (z-component)");
  }
  m_fWfield->Draw("SURF4");
  m_canvas->Update();
}

void ViewField::PlotContourWeightingField(const std::string& label,
                                          const std::string& option) {

  m_electrode = label;

  SetupCanvas(true);
  if (!m_fWfield) CreateFunctionWeightingField();
  const int plotType = SetupFunction(option, m_fWfield);
  if (plotType == 0) {
    m_fWfield->SetMinimum(0.);
    m_fWfield->SetMaximum(1.);
  } else {
    m_fWfield->SetMinimum(m_wmin);
    m_fWfield->SetMaximum(m_wmax);
  }

  double level[nMaxContours];
  for (unsigned int i = 0; i < m_nContours; ++i) {
    if (m_nContours > 1) {
      level[i] = i / (m_nContours - 1.);
      if (plotType > 0) {
        level[i] = m_wmin + (m_wmax - m_wmin) * level[i];
      }
    } else {
      level[i] = 0.5;
      if (plotType > 0) {
        level[i] *= (m_wmax + m_wmin);
      }
    }
  }
  m_fWfield->SetContour(m_nContours, level);

  if (m_debug) {
    std::cout << m_className << "::PlotContour:\n";
    std::cout << "    Number of contours: " << m_nContours << "\n";
    for (unsigned int i = 0; i < m_nContours; ++i) {
      std::cout << "        Level " << i << " = " << level[i] << "\n";
    }
  }
  if (plotType == 0) {
    m_fWfield->SetTitle("Contours of the weighting potential");
  } else if (plotType == 1) {
    m_fWfield->SetTitle("Contours of the weighting field");
  } else if (plotType == 2) {
    m_fWfield->SetTitle("Contours of the weighting field (x-component)");
  } else if (plotType == 3) {
    m_fWfield->SetTitle("Contours of the weighting field (y-component)");
  } else if (plotType == 4) {
    m_fWfield->SetTitle("Contours of the weighting field (z-component)");
  }
  m_fWfield->Draw("CONT4Z");
  m_canvas->Update();
}

void ViewField::CreateFunction() {

  int idx = 0;
  std::string fname = "fPotential_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "fPotential_";
    ss << idx;
    fname = ss.str();
  }

  m_fPot = new TF2(fname.c_str(), this, &ViewField::EvaluatePotential, 
                   m_pxmin, m_pxmax, m_pymin, m_pymax, 1, 
                   "ViewField", "EvaluatePotential");
}

void ViewField::CreateProfileFunction() {

  int idx = 0;
  std::string fname = "fPotentialProfile_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "fPotentialProfile_";
    ss << idx;
    fname = ss.str();
  }

  const int nParameters = 7;
  m_fPotProfile =
      new TF1(fname.c_str(), this, &ViewField::EvaluatePotentialProfile, 0., 1.,
              nParameters, "ViewField", "EvaluatePotentialProfile");
}

void ViewField::CreateFunctionWeightingField() {

  int idx = 0;
  std::string fname = "fWfield_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "fWfield_";
    ss << idx;
    fname = ss.str();
  }

  m_fWfield =
      new TF2(fname.c_str(), this, &ViewField::EvaluateWeightingField, 
              m_pxmin, m_pxmax, m_pymin, m_pymax, 1, 
              "ViewField", "EvaluateWeightingField");
}

void ViewField::SetDefaultProjection() {

  // Default projection: x-y at z=0
  m_project[0][0] = 1;
  m_project[1][0] = 0;
  m_project[2][0] = 0;
  m_project[0][1] = 0;
  m_project[1][1] = 1;
  m_project[2][1] = 0;
  m_project[0][2] = 0;
  m_project[1][2] = 0;
  m_project[2][2] = 0;

  // Plane description
  m_plane[0] = 0;
  m_plane[1] = 0;
  m_plane[2] = 1;
  m_plane[3] = 0;

  // Prepare axis labels.
  Labels();
}

double ViewField::EvaluatePotential(double* pos, double* par) {

  if (!m_sensor && !m_component) return 0.;

  // Compute the field.
  double ex = 0., ey = 0., ez = 0., volt = 0.;
  int status = 0;
  Medium* medium = NULL;
  const double xpos =
      m_project[0][0] * pos[0] + m_project[1][0] * pos[1] + m_project[2][0];
  const double ypos =
      m_project[0][1] * pos[0] + m_project[1][1] * pos[1] + m_project[2][1];
  const double zpos =
      m_project[0][2] * pos[0] + m_project[1][2] * pos[1] + m_project[2][2];

  if (!m_sensor) {
    m_component->ElectricField(xpos, ypos, zpos, ex, ey, ez, volt, medium,
                               status);
  } else {
    m_sensor->ElectricField(xpos, ypos, zpos, ex, ey, ez, volt, medium, status);
  }
  if (m_debug) {
    std::cout << m_className << "::EvaluatePotential:\n";
    std::cout << "    At (u, v) = (" << pos[0] << ", " << pos[1] << "), "
              << " (x,y,z) = (" << xpos << "," << ypos << "," << zpos << ")\n";
    std::cout << "    E = " << ex << ", " << ey << ", " << ez
              << "), V = " << volt << ", status = " << status << "\n";
  }

  if (m_useStatus && status != 0) return m_vBkg;

  // Select the quantity to be plotted.
  if (par[0] > 30.) {
    return ez;
  } else if (par[0] > 20.) {
    return ey;
  } else if (par[0] > 10.) {
    return ex;
  } else if (par[0] > 0.) {
    return sqrt(ex * ex + ey * ey + ez * ez);
  }
  // Return the potential.
  return volt;
}

double ViewField::EvaluatePotentialProfile(double* pos, double* par) {

  if (!m_sensor && !m_component) return 0.;

  // Get the start and end position.
  const double x0 = par[0];
  const double y0 = par[1];
  const double z0 = par[2];
  const double x1 = par[3];
  const double y1 = par[4];
  const double z1 = par[5];
  // Compute the direction.
  const double dx = x1 - x0;
  const double dy = y1 - y0;
  const double dz = z1 - z0;
  // Get the position.
  const double t = pos[0];

  // Compute the field.
  double ex = 0., ey = 0., ez = 0., volt = 0.;
  int status = 0;
  Medium* medium = NULL;
  if (!m_sensor) {
    m_component->ElectricField(x0 + t * dx, y0 + t * dy, z0 + t * dz, 
                               ex, ey, ez, volt, medium, status);
  } else {
    m_sensor->ElectricField(x0 + t * dx, y0 + t * dy, z0 + t * dz, 
                            ex, ey, ez, volt, medium, status);
  }

  if (m_useStatus && status != 0) volt = m_vBkg;

  // Select the quantity to be plotted.
  if (par[6] > 30.) {
    return ez;
  } else if (par[6] > 20.) {
    return ey;
  } else if (par[6] > 10.) {
    return ex;
  } else if (par[6] > 0.) {
    return sqrt(ex * ex + ey * ey + ez * ez);
  }
  // Return the potential.
  return volt;
}

double ViewField::EvaluateWeightingField(double* pos, double* par) {

  if (!m_sensor && !m_component) return 0.;

  // Compute the field.
  double ex = 0., ey = 0., ez = 0., v = 0.;
  const double xpos =
      m_project[0][0] * pos[0] + m_project[1][0] * pos[1] + m_project[2][0];
  const double ypos =
      m_project[0][1] * pos[0] + m_project[1][1] * pos[1] + m_project[2][1];
  const double zpos =
      m_project[0][2] * pos[0] + m_project[1][2] * pos[1] + m_project[2][2];

  if (!m_sensor) {
    if (par[0] > 0.) {
      m_component->WeightingField(xpos, ypos, zpos, ex, ey, ez, m_electrode);
    } else {
      v = m_component->WeightingPotential(xpos, ypos, zpos, m_electrode);
    }
  } else {
    if (par[0] > 0.) {
      m_sensor->WeightingField(xpos, ypos, zpos, ex, ey, ez, m_electrode);
    } else {
      v = m_sensor->WeightingPotential(xpos, ypos, zpos, m_electrode);
    }
  }
  if (m_debug) {
    std::cout << m_className << "::EvaluateWeightingField:\n";
    std::cout << "    At (u, v) = (" << pos[0] << ", " << pos[1] << "), "
              << " (x, y, z) = (" << xpos << "," << ypos << "," << zpos
              << ")\n";
    if (par[0] > 0.) {
      std::cout << "    E = (" << ex << ", " << ey << ", " << ez << ")\n";
    } else {
      std::cout << "    V = " << v << "\n";
    }
  }

  // Select the quantity to be plotted.
  if (par[0] < 0.) {
    return v;
  } else if (par[0] > 30.) {
    return ez;
  } else if (par[0] > 20.) {
    return ey;
  } else if (par[0] > 10.) {
    return ex;
  } else {
    return sqrt(ex * ex + ey * ey + ez * ez);
  }
}

void ViewField::Labels() {

  // Initialisation of the x-axis label
  strcpy(m_xLabel, "\0");
  char buf[100];

  // x portion
  if (fabs(m_project[0][0] - 1) < 1.0e-4) {
    strcat(m_xLabel, "x");
  } else if (fabs(m_project[0][0] + 1) < 1.0e-4) {
    strcat(m_xLabel, "-x");
  } else if (m_project[0][0] > 1.0e-4) {
    sprintf(buf, "%g x", m_project[0][0]);
    strcat(m_xLabel, buf);
  } else if (m_project[0][0] < -1.0e-4) {
    sprintf(buf, "%g x", m_project[0][0]);
    strcat(m_xLabel, buf);
  }

  // y portion
  if (strlen(m_xLabel) > 0) {
    if (m_project[0][1] < -1.0e-4) {
      strcat(m_xLabel, " - ");
    } else if (m_project[0][1] > 1.0e-4) {
      strcat(m_xLabel, " + ");
    }
    if (fabs(m_project[0][1] - 1) < 1.0e-4 || fabs(m_project[0][1] + 1) < 1.0e-4) {
      strcat(m_xLabel, "y");
    } else if (fabs(m_project[0][1]) > 1.0e-4) {
      sprintf(buf, "%g y", fabs(m_project[0][1]));
      strcat(m_xLabel, buf);
    }
  } else {
    if (fabs(m_project[0][1] - 1) < 1.0e-4) {
      strcat(m_xLabel, "y");
    } else if (fabs(m_project[0][1] + 1) < 1.0e-4) {
      strcat(m_xLabel, "-y");
    } else if (m_project[0][1] > 1.0e-4) {
      sprintf(buf, "%g y", m_project[0][1]);
      strcat(m_xLabel, buf);
    } else if (m_project[0][1] < -1.0e-4) {
      sprintf(buf, "%g y", m_project[0][1]);
      strcat(m_xLabel, buf);
    }
  }

  // z portion
  if (strlen(m_xLabel) > 0) {
    if (m_project[0][2] < -1.0e-4) {
      strcat(m_xLabel, " - ");
    } else if (m_project[0][2] > 1.0e-4) {
      strcat(m_xLabel, " + ");
    }
    if (fabs(m_project[0][2] - 1) < 1.0e-4 || fabs(m_project[0][2] + 1) < 1.0e-4) {
      strcat(m_xLabel, "z");
    } else if (fabs(m_project[0][2]) > 1.0e-4) {
      sprintf(buf, "%g z", fabs(m_project[0][2]));
      strcat(m_xLabel, buf);
    }
  } else {
    if (fabs(m_project[0][2] - 1) < 1.0e-4) {
      strcat(m_xLabel, "z");
    } else if (fabs(m_project[0][2] + 1) < 1.0e-4) {
      strcat(m_xLabel, "-z");
    } else if (m_project[0][2] > 1.0e-4) {
      sprintf(buf, "%g z", m_project[0][2]);
      strcat(m_xLabel, buf);
    } else if (m_project[0][2] < -1.0e-4) {
      sprintf(buf, "%g z", m_project[0][2]);
      strcat(m_xLabel, buf);
    }
  }

  // Unit
  strcat(m_xLabel, " [cm]");

  // Initialisation of the y-axis label
  strcpy(m_yLabel, "\0");

  // x portion
  if (fabs(m_project[1][0] - 1) < 1.0e-4) {
    strcat(m_yLabel, "x");
  } else if (fabs(m_project[1][0] + 1) < 1.0e-4) {
    strcat(m_yLabel, "-x");
  } else if (m_project[1][0] > 1.0e-4) {
    sprintf(buf, "%g x", m_project[1][0]);
    strcat(m_yLabel, buf);
  } else if (m_project[1][0] < -1.0e-4) {
    sprintf(buf, "%g x", m_project[1][0]);
    strcat(m_yLabel, buf);
  }

  // y portion
  if (strlen(m_yLabel) > 0) {
    if (m_project[1][1] < -1.0e-4) {
      strcat(m_yLabel, " - ");
    } else if (m_project[1][1] > 1.0e-4) {
      strcat(m_yLabel, " + ");
    }
    if (fabs(m_project[1][1] - 1) < 1.0e-4 || fabs(m_project[1][1] + 1) < 1.0e-4) {
      strcat(m_yLabel, "y");
    } else if (fabs(m_project[1][1]) > 1.0e-4) {
      sprintf(buf, "%g y", fabs(m_project[1][1]));
      strcat(m_yLabel, buf);
    }
  } else {
    if (fabs(m_project[1][1] - 1) < 1.0e-4) {
      strcat(m_yLabel, "y");
    } else if (fabs(m_project[1][1] + 1) < 1.0e-4) {
      strcat(m_yLabel, "-y");
    } else if (m_project[1][1] > 1.0e-4) {
      sprintf(buf, "%g y", m_project[1][1]);
      strcat(m_yLabel, buf);
    } else if (m_project[1][1] < -1.0e-4) {
      sprintf(buf, "%g y", m_project[1][1]);
      strcat(m_yLabel, buf);
    }
  }

  // z portion
  if (strlen(m_yLabel) > 0) {
    if (m_project[1][2] < -1.0e-4) {
      strcat(m_yLabel, " - ");
    } else if (m_project[1][2] > 1.0e-4) {
      strcat(m_yLabel, " + ");
    }
    if (fabs(m_project[1][2] - 1) < 1.0e-4 || fabs(m_project[1][2] + 1) < 1.0e-4) {
      strcat(m_yLabel, "z");
    } else if (fabs(m_project[1][2]) > 1.0e-4) {
      sprintf(buf, "%g z", fabs(m_project[1][2]));
      strcat(m_yLabel, buf);
    }
  } else {
    if (fabs(m_project[1][2] - 1) < 1.0e-4) {
      strcat(m_yLabel, "z");
    } else if (fabs(m_project[1][2] + 1) < 1.0e-4) {
      strcat(m_yLabel, "-z");
    } else if (m_project[1][2] > 1.0e-4) {
      sprintf(buf, "%g z", m_project[1][2]);
      strcat(m_yLabel, buf);
    } else if (m_project[1][2] < -1.0e-4) {
      sprintf(buf, "%g z", m_project[1][2]);
      strcat(m_yLabel, buf);
    }
  }

  // Unit
  strcat(m_yLabel, " [cm]");

  // Initialisation of the y-axis label
  strcpy(m_description, "\0");

  // x portion
  if (fabs(m_plane[0] - 1) < 1.0e-4) {
    strcat(m_description, "x");
  } else if (fabs(m_plane[0] + 1) < 1.0e-4) {
    strcat(m_description, "-x");
  } else if (m_plane[0] > 1.0e-4) {
    sprintf(buf, "%g x", m_plane[0]);
    strcat(m_description, buf);
  } else if (m_plane[0] < -1.0e-4) {
    sprintf(buf, "%g x", m_plane[0]);
    strcat(m_description, buf);
  }

  // y portion
  if (strlen(m_description) > 0) {
    if (m_plane[1] < -1.0e-4) {
      strcat(m_description, " - ");
    } else if (m_plane[1] > 1.0e-4) {
      strcat(m_description, " + ");
    }
    if (fabs(m_plane[1] - 1) < 1.0e-4 || fabs(m_plane[1] + 1) < 1.0e-4) {
      strcat(m_description, "y");
    } else if (fabs(m_plane[1]) > 1.0e-4) {
      sprintf(buf, "%g y", fabs(m_plane[1]));
      strcat(m_description, buf);
    }
  } else {
    if (fabs(m_plane[1] - 1) < 1.0e-4) {
      strcat(m_description, "y");
    } else if (fabs(m_plane[1] + 1) < 1.0e-4) {
      strcat(m_description, "-y");
    } else if (m_plane[1] > 1.0e-4) {
      sprintf(buf, "%g y", m_plane[1]);
      strcat(m_description, buf);
    } else if (m_plane[1] < -1.0e-4) {
      sprintf(buf, "%g y", m_plane[1]);
      strcat(m_description, buf);
    }
  }

  // z portion
  if (strlen(m_description) > 0) {
    if (m_plane[2] < -1.0e-4) {
      strcat(m_description, " - ");
    } else if (m_plane[2] > 1.0e-4) {
      strcat(m_description, " + ");
    }
    if (fabs(m_plane[2] - 1) < 1.0e-4 || fabs(m_plane[2] + 1) < 1.0e-4) {
      strcat(m_description, "z");
    } else if (fabs(m_plane[2]) > 1.0e-4) {
      sprintf(buf, "%g z", fabs(m_plane[2]));
      strcat(m_description, buf);
    }
  } else {
    if (fabs(m_plane[2] - 1) < 1.0e-4) {
      strcat(m_description, "z");
    } else if (fabs(m_plane[2] + 1) < 1.0e-4) {
      strcat(m_description, "-z");
    } else if (m_plane[2] > 1.0e-4) {
      sprintf(buf, "%g z", m_plane[2]);
      strcat(m_description, buf);
    } else if (m_plane[2] < -1.0e-4) {
      sprintf(buf, "%g z", m_plane[2]);
      strcat(m_description, buf);
    }
  }

  // Constant
  sprintf(buf, " = %g", m_plane[3]);
  strcat(m_description, buf);

  if (m_debug) {
    std::cout << m_className << "::Labels:\n";
    std::cout << "    x label: |" << m_xLabel << "|\n";
    std::cout << "    y label: |" << m_yLabel << "|\n";
    std::cout << "    plane:   |" << m_description << "|\n";
  }
}

void ViewField::SetPlane(const double fx, const double fy, const double fz,
                         const double x0, const double y0, const double z0) {

  // Calculate 2 in-plane vectors for the normal vector
  double fnorm = sqrt(fx * fx + fy * fy + fz * fz);
  if (fnorm > 0 && fx * fx + fz * fz > 0) {
    m_project[0][0] = fz / sqrt(fx * fx + fz * fz);
    m_project[0][1] = 0;
    m_project[0][2] = -fx / sqrt(fx * fx + fz * fz);
    m_project[1][0] = -fx * fy / (sqrt(fx * fx + fz * fz) * fnorm);
    m_project[1][1] = (fx * fx + fz * fz) / (sqrt(fx * fx + fz * fz) * fnorm);
    m_project[1][2] = -fy * fz / (sqrt(fx * fx + fz * fz) * fnorm);
    m_project[2][0] = x0;
    m_project[2][1] = y0;
    m_project[2][2] = z0;
  } else if (fnorm > 0 && fy * fy + fz * fz > 0) {
    m_project[0][0] = (fy * fy + fz * fz) / (sqrt(fy * fy + fz * fz) * fnorm);
    m_project[0][1] = -fx * fz / (sqrt(fy * fy + fz * fz) * fnorm);
    m_project[0][2] = -fy * fz / (sqrt(fy * fy + fz * fz) * fnorm);
    m_project[1][0] = 0;
    m_project[1][1] = fz / sqrt(fy * fy + fz * fz);
    m_project[1][2] = -fy / sqrt(fy * fy + fz * fz);
    m_project[2][0] = x0;
    m_project[2][1] = y0;
    m_project[2][2] = z0;
  } else {
    std::cout << m_className << "::SetPlane:\n";
    std::cout << "    Normal vector has zero norm.\n";
    std::cout << "    No new projection set.\n";
  }

  // Store the plane description
  m_plane[0] = fx;
  m_plane[1] = fy;
  m_plane[2] = fz;
  m_plane[3] = fx * x0 + fy * y0 + fz * z0;

  // Make labels to be placed along the axes
  Labels();
}

void ViewField::Rotate(const double angle) {

  // Rotate the axes
  double auxu[3], auxv[3];
  for (int i = 0; i < 3; ++i) {
    auxu[i] = cos(angle) * m_project[0][i] - sin(angle) * m_project[1][i];
    auxv[i] = sin(angle) * m_project[0][i] + cos(angle) * m_project[1][i];
  }
  for (int i = 0; i < 3; ++i) {
    m_project[0][i] = auxu[i];
    m_project[1][i] = auxv[i];
  }

  // Make labels to be placed along the axes
  Labels();
}

void ViewField::SetupCanvas(const bool twod) {

  if (!m_canvas) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle("Field View");
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();
  if (twod) m_canvas->Range(m_pxmin, m_pymin, m_pxmax, m_pymax);
}

int ViewField::SetupFunction(const std::string& option, TF2* f) {

  int plotType = 0;
  if (option == "v" || option == "p" || option == "phi" || option == "volt" ||
      option == "voltage" || option == "pot" || option == "potential") {
    f->SetParameter(0, -1.);
    f->SetRange(m_pxmin, m_pymin, m_pxmax, m_pymax);
    f->SetMinimum(m_fmin);
    f->SetMaximum(m_fmax);
    plotType = 0;
  } else if (option == "e" || option == "field") {
    f->SetParameter(0, 1.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = 1;
  } else if (option == "ex") {
    f->SetParameter(0, 11.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = 2;
  } else if (option == "ey") {
    f->SetParameter(0, 21.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = 3;
  } else if (option == "ez") {
    f->SetParameter(0, 31.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = 4;
  } else { 
    std::cerr << m_className << "::SetupFunction:\n";
    std::cerr << "    Unknown option (" << option << ")\n";
    std::cerr << "    Plotting the potential.\n";
    f->SetParameter(0, -1.);
    f->SetMinimum(m_fmin);
    f->SetMaximum(m_fmax);
  }
  f->SetNpx(m_nSamples2dX);
  f->SetNpy(m_nSamples2dY);
  f->GetXaxis()->SetTitle(m_xLabel);
  f->GetYaxis()->SetTitle(m_yLabel);
  return plotType;
}

}
