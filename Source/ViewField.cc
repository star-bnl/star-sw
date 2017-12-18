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
      m_nContours(m_nMaxContours),
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
    std::cerr << m_className << "::SetSensor: Null pointer.\n";
    return;
  }

  m_sensor = s;
  m_component = NULL;
  // Get the bounding box.
  bool ok = m_sensor->GetArea(m_pxmin, m_pymin, m_pzmin, m_pxmax, m_pymax, m_pzmax);
  if (!ok) {
    std::cerr << m_className << "::SetSensor:\n"
              << "    Warning: bounding box of sensor is not defined.\n";
  }
  // Get the voltage range.
  ok = m_sensor->GetVoltageRange(m_fmin, m_fmax);
  if (!ok) {
    std::cerr << m_className << "::SetSensor:\n"
              << "    Warning: voltage range of sensor is not defined.\n";
  }
}

void ViewField::SetComponent(ComponentBase* c) {

  if (!c) {
    std::cerr << m_className << "::SetComponent: Null pointer.\n";
    return;
  }

  m_component = c;
  m_sensor = NULL;
  // Get the bounding box.
  bool ok = m_component->GetBoundingBox(m_pxmin, m_pymin, m_pzmin, 
                                        m_pxmax, m_pymax, m_pzmax);
  if (!ok) {
    std::cerr << m_className << "::SetComponent:\n"
              << "    Warning: bounding box of component is not defined.\n";
  }
  // Get the voltage range.
  ok = m_component->GetVoltageRange(m_fmin, m_fmax);
  if (!ok) {
    std::cerr << m_className << "::SetComponent:\n"
              << "    Warning: voltage range of component is not defined.\n";
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

  // Check range, assign if non-null.
  if (xmin == xmax || ymin == ymax) {
    std::cerr << m_className << "::SetArea: Null area range is not permitted.\n"
              << "      " << xmin << " < x < " << xmax << "\n"
              << "      " << ymin << " < y < " << ymax << "\n";
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

  if (n <= m_nMaxContours) {
    m_nContours = n;
  } else {
    std::cerr << m_className << "::SetNumberOfContours:\n"
              << "    Max. number of contours is " << m_nMaxContours << ".\n";
  }
}

void ViewField::SetNumberOfSamples1d(const unsigned int n) {

  const unsigned int nmin = 10;
  const unsigned int nmax = 100000;

  if (n < nmin || n > nmax) {
    std::cerr << m_className << "::SetNumberOfSamples1d:\n"
              << "    Number of points (" << n << ") out of range.\n"
              << "    " << nmin << " <= n <= " << nmax << "\n";
    return;
  }

  m_nSamples1d = n;
}

void ViewField::SetNumberOfSamples2d(const unsigned int nx, 
                                     const unsigned int ny) {

  const unsigned int nmin = 10;
  const unsigned int nmax = 10000;
  if (nx < nmin || nx > nmax) {
    std::cerr << m_className << "::SetNumberOfSamples2d:\n"
              << "    Number of x-points (" << nx << ") out of range.\n"
              << "    " << nmin << " <= nx <= " << nmax << "\n";
  } else {
    m_nSamples2dX = nx;
  }

  if (ny < nmin || ny > nmax) {
    std::cerr << m_className << "::SetNumberOfSamples2d:\n"
              << "    Number of y-points (" << ny << ") out of range.\n"
              << "    " << nmin << " <= ny <= " << nmax << "\n";
  } else {
    m_nSamples2dY = ny;
  }
}

void ViewField::PlotContour(const std::string& option) {

  SetupCanvas();
  if (!m_fPot) CreateFunction();
  const PlotType plotType = SetupFunction(option, m_fPot);

  double level[m_nMaxContours];
  const double ymin = plotType == Potential ? m_fmin : m_emin;
  const double ymax = plotType == Potential ? m_fmax : m_emax;
  if (m_nContours > 1) {
    const double ystep = (ymax - ymin) / (m_nContours - 1.);
    for (unsigned int i = 0; i < m_nContours; ++i) {
      level[i] = ymin + i * ystep;
    } 
  } else {
   level[0] = 0.5 * (ymax + ymin);
  }
  m_fPot->SetContour(m_nContours, level);

  if (m_debug) {
    std::cout << m_className << "::PlotContour:\n"
              << "    Number of contours: " << m_nContours << "\n";
    for (unsigned int i = 0; i < m_nContours; ++i) {
      std::cout << "        Level " << i << " = " << level[i] << "\n";
    }
  }
  switch (plotType) {
    case Potential:
      m_fPot->SetTitle("Contours of the potential");
      break;
    case Magnitude:
      m_fPot->SetTitle("Contours of the electric field");
      break;
    case Ex:
      m_fPot->SetTitle("Contours of the electric field (x-component)");
      break;
    case Ey:
      m_fPot->SetTitle("Contours of the electric field (y-component)");
      break;
    case Ez:
      m_fPot->SetTitle("Contours of the electric field (z-component)");
      break;
    default:
      break;
  }
  m_fPot->Draw("CONT4Z");
  gPad->SetRightMargin(0.15);
  gPad->Update();
}

void ViewField::PlotSurface(const std::string& option) {

  Plot(option, "SURF4");
}

void ViewField::Plot(const std::string& option, 
                     const std::string& drawopt) {

  SetupCanvas();
  if (!m_fPot) CreateFunction();
  const PlotType plotType = SetupFunction(option, m_fPot);
  switch (plotType) {
    case Potential:
      m_fPot->SetTitle("Potential");
      break;
    case Magnitude:
      m_fPot->SetTitle("Electric field");
      break;
    case Ex:
      m_fPot->SetTitle("Electric field (x-component)");
      break;
    case Ey:
      m_fPot->SetTitle("Electric field (y-component)");
      break;
    case Ez:
      m_fPot->SetTitle("Electric field (z-component)");
      break;
    default:
      break;
  }
  m_fPot->Draw(drawopt.c_str());
  gPad->Update();
}

void ViewField::PlotProfile(const double x0, const double y0, const double z0,
                            const double x1, const double y1, const double z1,
                            const std::string& option) {

  // Check the distance between the two points.
  const double d = sqrt(pow(x1 - x0, 2) + pow(y1 - y0, 2) + pow(z1 - z0, 2));
  if (d <= 0.) {
    std::cerr << m_className << "::PlotProfile:\n"
              << "    Start and end points coincide.\n";
    return;
  }

  SetupCanvas();
  if (!m_fPotProfile) CreateProfileFunction();

  m_fPotProfile->SetParameter(0, x0);
  m_fPotProfile->SetParameter(1, y0);
  m_fPotProfile->SetParameter(2, z0);
  m_fPotProfile->SetParameter(3, x1);
  m_fPotProfile->SetParameter(4, y1);
  m_fPotProfile->SetParameter(5, z1);
  PlotType plotType = Potential;
  if (option == "v" || option == "p" || option == "phi" || option == "volt" ||
      option == "voltage" || option == "pot" || option == "potential") {
    m_fPotProfile->SetParameter(6, -1.);
    m_fPotProfile->SetMinimum(m_fmin);
    m_fPotProfile->SetMaximum(m_fmax);
  } else if (option == "e" || option == "field") {
    m_fPotProfile->SetParameter(6, 1.);
    plotType = Magnitude;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else if (option == "ex") {
    m_fPotProfile->SetParameter(6, 11.);
    plotType = Ex;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else if (option == "ey") {
    m_fPotProfile->SetParameter(6, 21.);
    plotType = Ey;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else if (option == "ez") {
    m_fPotProfile->SetParameter(6, 31.);
    plotType = Ez;
    m_fPotProfile->SetMinimum(m_emin);
    m_fPotProfile->SetMaximum(m_emax);
  } else {
    std::cerr << m_className << "::PlotProfile:\n    Unknown option ("
              << option << "). Plotting the potential.\n";
    m_fPotProfile->SetParameter(6, -1.);
    m_fPotProfile->SetMinimum(m_fmin);
    m_fPotProfile->SetMaximum(m_fmax);
  }
  if (m_debug) {
    std::cout << m_className << "::PlotProfile:\n";
    if (plotType == Potential) {
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
  switch (plotType) {
    case Potential:
      m_fPotProfile->SetTitle("Profile plot of the potential");
      m_fPotProfile->GetYaxis()->SetTitle("potential [V]");
      break;
    case Magnitude:
      m_fPotProfile->SetTitle("Profile plot of the electric field");
      m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
      break;
    case Ex:
      m_fPotProfile->SetTitle("Profile plot of the electric field (x-component)");
      m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
      break;
    case Ey:
      m_fPotProfile->SetTitle("Profile plot of the electric field (y-component)");
      m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
      break;
    case Ez:
      m_fPotProfile->SetTitle("Profile plot of the electric field (z-component)");
      m_fPotProfile->GetYaxis()->SetTitle("field [V/cm]");
      break;
    default:
      break;
  }
  m_fPotProfile->SetNpx(m_nSamples1d);
  m_fPotProfile->Draw();
  gPad->Update();
}

void ViewField::PlotSurfaceWeightingField(const std::string& label,
                                          const std::string& option) {

  m_electrode = label;

  SetupCanvas();
  if (!m_fWfield) CreateFunctionWeightingField();
  const PlotType plotType = SetupFunction(option, m_fWfield);
  switch (plotType) {
    case Potential:
      m_fWfield->SetTitle("Surface plot of the weighting potential");
      break;
    case Magnitude:
      m_fWfield->SetTitle("Surface plot of the weighting field");
      break;
    case Ex:
      m_fWfield->SetTitle("Surface plot of the weighting field (x-component)");
      break;
    case Ey:
      m_fWfield->SetTitle("Surface plot of the weighting field (y-component)");
      break;
    case Ez:
      m_fWfield->SetTitle("Surface plot of the weighting field (z-component)");
      break;
    default:
      break;
  }
  m_fWfield->Draw("SURF4");
  m_canvas->Update();
}

void ViewField::PlotContourWeightingField(const std::string& label,
                                          const std::string& option) {

  m_electrode = label;

  SetupCanvas();
  if (!m_fWfield) CreateFunctionWeightingField();
  const PlotType plotType = SetupFunction(option, m_fWfield);
  if (plotType == Potential) {
    m_fWfield->SetMinimum(0.);
    m_fWfield->SetMaximum(1.);
  } else {
    m_fWfield->SetMinimum(m_wmin);
    m_fWfield->SetMaximum(m_wmax);
  }

  double level[m_nMaxContours];
  if (m_nContours > 1) {
    const double wrange = plotType == Potential ? 1. : m_wmax - m_wmin;
    const double wstep = wrange / (m_nContours - 1.);
    const double w0 = plotType == Potential ? 0. : m_wmin;
    for (unsigned int i = 0; i < m_nContours; ++i) {
      level[i] = w0 + i * wstep;
    }
  } else {
    level[0] = plotType == Potential ? 0.5 : 0.5 * (m_wmax + m_wmin);
  }
  m_fWfield->SetContour(m_nContours, level);

  if (m_debug) {
    std::cout << m_className << "::PlotContourWeightingField:\n"
              << "    Number of contours: " << m_nContours << "\n";
    for (unsigned int i = 0; i < m_nContours; ++i) {
      std::cout << "        Level " << i << " = " << level[i] << "\n";
    }
  }
  switch (plotType) {
    case Potential:
      m_fWfield->SetTitle("Contours of the weighting potential");
      break;
    case Magnitude:
      m_fWfield->SetTitle("Contours of the weighting field");
      break;
    case Ex:
      m_fWfield->SetTitle("Contours of the weighting field (x-component)");
      break;
    case Ey:
      m_fWfield->SetTitle("Contours of the weighting field (y-component)");
      break;
    case Ez:
      m_fWfield->SetTitle("Contours of the weighting field (z-component)");
      break;
    default:
      break;
  }
  m_fWfield->Draw("CONT4Z");
  gPad->Update();
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
  const double u = pos[0];
  const double v = pos[1];
  const double x = m_project[0][0] * u + m_project[1][0] * v + m_project[2][0];
  const double y = m_project[0][1] * u + m_project[1][1] * v + m_project[2][1];
  const double z = m_project[0][2] * u + m_project[1][2] * v + m_project[2][2];

  if (!m_sensor) {
    m_component->ElectricField(x, y, z, ex, ey, ez, volt, medium, status);
  } else {
    m_sensor->ElectricField(x, y, z, ex, ey, ez, volt, medium, status);
  }
  if (m_debug) {
    std::cout << m_className << "::EvaluatePotential:\n"
              << "    At (u, v) = (" << u << ", " << v << "), "
              << " (x,y,z) = (" << x << "," << y << "," << z << ")\n"
              << "    E = " << ex << ", " << ey << ", " << ez
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
  double ex = 0., ey = 0., ez = 0., volt = 0.;
  const double u = pos[0];
  const double v = pos[1];
  const double x = m_project[0][0] * u + m_project[1][0] * v + m_project[2][0];
  const double y = m_project[0][1] * u + m_project[1][1] * v + m_project[2][1];
  const double z = m_project[0][2] * u + m_project[1][2] * v + m_project[2][2];

  if (!m_sensor) {
    if (par[0] > 0.) {
      m_component->WeightingField(x, y, z, ex, ey, ez, m_electrode);
    } else {
      volt = m_component->WeightingPotential(x, y, z, m_electrode);
    }
  } else {
    if (par[0] > 0.) {
      m_sensor->WeightingField(x, y, z, ex, ey, ez, m_electrode);
    } else {
      volt = m_sensor->WeightingPotential(x, y, z, m_electrode);
    }
  }
  if (m_debug) {
    std::cout << m_className << "::EvaluateWeightingField:\n"
              << "    At (u, v) = (" << u << ", " << v << "), "
              << " (x, y, z) = (" << x << "," << y << "," << z << ")\n";
    if (par[0] > 0.) {
      std::cout << "    E = (" << ex << ", " << ey << ", " << ez << ")\n";
    } else {
      std::cout << "    V = " << volt << "\n";
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
    std::cout << m_className << "::SetPlane:\n"
              << "    Normal vector has zero norm. No new projection set.\n";
  }

  // Store the plane description
  m_plane[0] = fx;
  m_plane[1] = fy;
  m_plane[2] = fz;
  m_plane[3] = fx * x0 + fy * y0 + fz * z0;

  // Make labels to be placed along the axes
  Labels();
}

void ViewField::Rotate(const double theta) {

  // Rotate the axes
  double auxu[3], auxv[3];
  const double ctheta = cos(theta);
  const double stheta = sin(theta);
  for (int i = 0; i < 3; ++i) {
    auxu[i] = ctheta * m_project[0][i] - stheta * m_project[1][i];
    auxv[i] = stheta * m_project[0][i] + ctheta * m_project[1][i];
  }
  for (int i = 0; i < 3; ++i) {
    m_project[0][i] = auxu[i];
    m_project[1][i] = auxv[i];
  }

  // Make labels to be placed along the axes
  Labels();
}

void ViewField::SetupCanvas() {

  if (!m_canvas) {
    m_canvas = new TCanvas();
    m_canvas->SetTitle("Field View");
    if (m_hasExternalCanvas) m_hasExternalCanvas = false;
  }
  m_canvas->cd();
}

ViewField::PlotType ViewField::SetupFunction(const std::string& option, TF2* f) {

  PlotType plotType = Unknown;
  if (option == "v" || option == "p" || option == "phi" || option == "volt" ||
      option == "voltage" || option == "pot" || option == "potential") {
    f->SetParameter(0, -1.);
    f->SetRange(m_pxmin, m_pymin, m_pxmax, m_pymax);
    f->SetMinimum(m_fmin);
    f->SetMaximum(m_fmax);
    plotType = Potential;
  } else if (option == "e" || option == "field") {
    f->SetParameter(0, 1.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = Magnitude;
  } else if (option == "ex") {
    f->SetParameter(0, 11.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = Ex;
  } else if (option == "ey") {
    f->SetParameter(0, 21.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = Ey;
  } else if (option == "ez") {
    f->SetParameter(0, 31.);
    f->SetMinimum(m_emin);
    f->SetMaximum(m_emax);
    plotType = Ez;
  } else { 
    std::cerr << m_className << "::SetupFunction:\n    Unknown option ("
              << option << "). Plotting the potential.\n";
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
