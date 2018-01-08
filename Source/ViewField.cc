#include <iostream>
#include <sstream>
#include <stdio.h>
#include <string.h>
#include <limits>
#include <cmath>

#include <TROOT.h>
#include <TAxis.h>

#include "Plotting.hh"
#include "Random.hh"
#include "Sensor.hh"
#include "ComponentBase.hh"
#include "ViewField.hh"

namespace {

void SampleRange(const double xmin, const double ymin, 
                 const double xmax, const double ymax, TF2* f,
                 double& zmin, double& zmax) {

  const unsigned int n = 1000;
  const double dx = xmax - xmin;
  const double dy = ymax - ymin;
  zmin = std::numeric_limits<double>::max();
  zmax = -zmin;
  for (unsigned int i = 0; i < n; ++i) {
    const double z = f->Eval(xmin + Garfield::RndmUniform() * dx, 
                             ymin + Garfield::RndmUniform() * dy);
    if (z < zmin) zmin = z;
    if (z > zmax) zmax = z;
  }
}

void SampleRange(TF1* f, double& ymin, double& ymax) {

  const unsigned int n = 1000;
  ymin = std::numeric_limits<double>::max();
  ymax = -ymin;
  for (unsigned int i = 0; i < n; ++i) {
    const double y = f->Eval(Garfield::RndmUniform());
    if (y < ymin) ymin = y;
    if (y > ymax) ymax = y;
  }
}

}

namespace Garfield {

ViewField::ViewField()
    : m_className("ViewField"),
      m_debug(false),
      m_useAutoRange(true),
      m_useStatus(false),
      m_vBkg(0.),
      m_sensor(NULL),
      m_component(NULL),
      m_hasUserArea(false),
      m_xmin(-1.),
      m_ymin(-1.),
      m_xmax(1.),
      m_ymax(1.),
      m_vmin(0.),
      m_vmax(100.),
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
      m_f2d(NULL),
      m_f2dW(NULL),
      m_fProfile(NULL),
      m_fProfileW(NULL) {

  SetDefaultProjection();
  plottingEngine.SetDefaultStyle();
}

ViewField::~ViewField() {

  if (!m_hasExternalCanvas && m_canvas) delete m_canvas;
  if (m_f2d) delete m_f2d;
  if (m_f2dW) delete m_f2dW;
  if (m_fProfile) delete m_fProfile;
  if (m_fProfileW) delete m_fProfileW;
}

void ViewField::SetSensor(Sensor* s) {

  if (!s) {
    std::cerr << m_className << "::SetSensor: Null pointer.\n";
    return;
  }

  m_sensor = s;
  m_component = NULL;
}

void ViewField::SetComponent(ComponentBase* c) {

  if (!c) {
    std::cerr << m_className << "::SetComponent: Null pointer.\n";
    return;
  }

  m_component = c;
  m_sensor = NULL;
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
  m_xmin = std::min(xmin, xmax);
  m_ymin = std::min(ymin, ymax);
  m_xmax = std::max(xmin, xmax);
  m_ymax = std::max(ymin, ymax);
  m_hasUserArea = true;
}

void ViewField::SetVoltageRange(const double vmin, const double vmax) {

  m_vmin = std::min(vmin, vmax);
  m_vmax = std::max(vmin, vmax);
  m_useAutoRange = false;
}

void ViewField::SetElectricFieldRange(const double emin, const double emax) {

  m_emin = std::min(emin, emax);
  m_emax = std::max(emin, emax);
  m_useAutoRange = false;
}

void ViewField::SetWeightingFieldRange(const double wmin, const double wmax) {

  m_wmin = std::min(wmin, wmax);
  m_wmax = std::max(wmin, wmax);
  m_useAutoRange = false;
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
  if (!SetupFunction(option, m_f2d, true, false)) return;
  m_f2d->Draw("CONT4Z");
  gPad->SetRightMargin(0.15);
  gPad->Update();
}

void ViewField::Plot(const std::string& option, const std::string& drawopt) {

  SetupCanvas();
  if (!SetupFunction(option, m_f2d, false, false)) return;
  m_f2d->Draw(drawopt.c_str());
  gPad->SetRightMargin(0.15);
  gPad->Update();
}

void ViewField::PlotProfile(const double x0, const double y0, const double z0,
                            const double x1, const double y1, const double z1,
                            const std::string& option) {


  SetupCanvas();
  if (!SetupProfile(x0, y0, z0, x1, y1, z1, option, m_fProfile, false)) return;
  m_fProfile->Draw();
  gPad->Update();
}

void ViewField::PlotWeightingField(const std::string& label,
                                   const std::string& option,
                                   const std::string& drawopt) {

  m_electrode = label;
  SetupCanvas();
  if (!SetupFunction(option, m_f2dW, false, true)) return;
  m_f2dW->Draw(drawopt.c_str());
  gPad->SetRightMargin(0.15);
  gPad->Update();
}

void ViewField::PlotContourWeightingField(const std::string& label,
                                          const std::string& option) {

  m_electrode = label;
  SetupCanvas();
  if (!SetupFunction(option, m_f2dW, true, true)) return;
  m_f2dW->Draw("CONT4Z");
  gPad->SetRightMargin(0.15);
  gPad->Update();
}

void ViewField::PlotProfileWeightingField(const std::string& label,
                            const double x0, const double y0, const double z0,
                            const double x1, const double y1, const double z1,
                            const std::string& option) {

  m_electrode = label;
  SetupCanvas();
  if (!SetupProfile(x0, y0, z0, x1, y1, z1, option, m_fProfileW, true)) return;
  m_fProfileW->Draw();
  gPad->Update();
}

std::string ViewField::FindUnusedFunctionName(const std::string& s) {

  int idx = 0;
  std::string fname = s + "_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << s;
    ss << idx;
    fname = ss.str();
  }
  return fname;
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

double ViewField::Evaluate2D(double* pos, double* par) {
  
  if (!m_sensor && !m_component) return 0.;

  // Transform to global coordinates.
  const double u = pos[0];
  const double v = pos[1];
  const double x = m_project[0][0] * u + m_project[1][0] * v + m_project[2][0];
  const double y = m_project[0][1] * u + m_project[1][1] * v + m_project[2][1];
  const double z = m_project[0][2] * u + m_project[1][2] * v + m_project[2][2];

  // Determine which quantity to plot.
  const PlotType plotType = static_cast<PlotType>(int(fabs(par[0]) / 10.));

  // Compute the field.
  double ex = 0., ey = 0., ez = 0., volt = 0.;
  int status = 0;
  if (par[0] > 0.) {
    // "Drift" electric field.
    Medium* medium = NULL;
    if (!m_sensor) {
      m_component->ElectricField(x, y, z, ex, ey, ez, volt, medium, status);
    } else {
      m_sensor->ElectricField(x, y, z, ex, ey, ez, volt, medium, status);
    }
  } else {
    // Weighting field.
    if (!m_sensor) {
      if (plotType == Potential) {
        volt = m_component->WeightingPotential(x, y, z, m_electrode);
      } else {
        m_component->WeightingField(x, y, z, ex, ey, ez, m_electrode);
      }
    } else {
      if (plotType == Potential) {
        volt = m_sensor->WeightingPotential(x, y, z, m_electrode);       
      } else {
        m_sensor->WeightingField(x, y, z, ex, ey, ez, m_electrode);
      }
    }
  }

  if (m_debug) {
    std::cout << m_className << "::Evaluate2D:\n"
              << "    At (u, v) = (" << u << ", " << v << "), "
              << " (x,y,z) = (" << x << "," << y << "," << z << ")\n"
              << "    E = " << ex << ", " << ey << ", " << ez
              << "), V = " << volt << ", status = " << status << "\n";
  }
  if (m_useStatus && status != 0) return m_vBkg;

  switch (plotType) {
    case Potential:
      return volt;
      break;
    case Magnitude:
      return sqrt(ex * ex + ey * ey + ez * ez);
      break;
    case Ex:
      return ex;
      break;
    case Ey:
      return ey;
      break;
    case Ez:
      return ez;
      break;
    default:
      break;
  }
  return volt;
}

double ViewField::EvaluateProfile(double* pos, double* par) {

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
  const double x = x0 + t * dx;
  const double y = y0 + t * dy;
  const double z = z0 + t * dz;
  // Determine which quantity to plot.
  const PlotType plotType = static_cast<PlotType>(int(fabs(par[6]) / 10.));

  // Compute the field.
  double ex = 0., ey = 0., ez = 0., volt = 0.;
  int status = 0;
  if (par[6] > 0.) {
    Medium* medium = NULL;
    // "Drift" electric field.
    if (!m_sensor) {
      m_component->ElectricField(x, y, z, ex, ey, ez, volt, medium, status);
    } else {
      m_sensor->ElectricField(x, y, z, ex, ey, ez, volt, medium, status);
    }
  } else {
    // Weighting field.
    if (!m_sensor) {
      if (plotType == Potential) {
        volt = m_component->WeightingPotential(x, y, z, m_electrode);
      } else {
        m_component->WeightingField(x, y, z, ex, ey, ez, m_electrode);
      }
    } else {
      if (plotType == Potential) {
        volt = m_sensor->WeightingPotential(x, y, z, m_electrode);       
      } else {
        m_sensor->WeightingField(x, y, z, ex, ey, ez, m_electrode);
      }
    }
  }
  if (m_useStatus && status != 0) volt = m_vBkg;

  switch (plotType) {
    case Potential:
      return volt;
      break;
    case Magnitude:
      return sqrt(ex * ex + ey * ey + ez * ez);
      break;
    case Ex:
      return ex;
      break;
    case Ey:
      return ey;
      break;
    case Ez:
      return ez;
      break;
    default:
      break;
  }
  return volt;
}

void ViewField::Labels() {

  // Initialisation of the x-axis label
  strcpy(m_xLabel, "\0");
  char buf[100];

  const double tol = 1.e-4;
  // x portion
  if (fabs(m_project[0][0] - 1) < tol) {
    strcat(m_xLabel, "x");
  } else if (fabs(m_project[0][0] + 1) < tol) {
    strcat(m_xLabel, "-x");
  } else if (m_project[0][0] > tol) {
    sprintf(buf, "%g x", m_project[0][0]);
    strcat(m_xLabel, buf);
  } else if (m_project[0][0] < -tol) {
    sprintf(buf, "%g x", m_project[0][0]);
    strcat(m_xLabel, buf);
  }

  // y portion
  if (strlen(m_xLabel) > 0) {
    if (m_project[0][1] < -tol) {
      strcat(m_xLabel, " - ");
    } else if (m_project[0][1] > tol) {
      strcat(m_xLabel, " + ");
    }
    if (fabs(m_project[0][1] - 1) < tol || fabs(m_project[0][1] + 1) < tol) {
      strcat(m_xLabel, "y");
    } else if (fabs(m_project[0][1]) > tol) {
      sprintf(buf, "%g y", fabs(m_project[0][1]));
      strcat(m_xLabel, buf);
    }
  } else {
    if (fabs(m_project[0][1] - 1) < tol) {
      strcat(m_xLabel, "y");
    } else if (fabs(m_project[0][1] + 1) < tol) {
      strcat(m_xLabel, "-y");
    } else if (m_project[0][1] > tol) {
      sprintf(buf, "%g y", m_project[0][1]);
      strcat(m_xLabel, buf);
    } else if (m_project[0][1] < -tol) {
      sprintf(buf, "%g y", m_project[0][1]);
      strcat(m_xLabel, buf);
    }
  }

  // z portion
  if (strlen(m_xLabel) > 0) {
    if (m_project[0][2] < -tol) {
      strcat(m_xLabel, " - ");
    } else if (m_project[0][2] > tol) {
      strcat(m_xLabel, " + ");
    }
    if (fabs(m_project[0][2] - 1) < tol || fabs(m_project[0][2] + 1) < tol) {
      strcat(m_xLabel, "z");
    } else if (fabs(m_project[0][2]) > tol) {
      sprintf(buf, "%g z", fabs(m_project[0][2]));
      strcat(m_xLabel, buf);
    }
  } else {
    if (fabs(m_project[0][2] - 1) < tol) {
      strcat(m_xLabel, "z");
    } else if (fabs(m_project[0][2] + 1) < tol) {
      strcat(m_xLabel, "-z");
    } else if (m_project[0][2] > tol) {
      sprintf(buf, "%g z", m_project[0][2]);
      strcat(m_xLabel, buf);
    } else if (m_project[0][2] < -tol) {
      sprintf(buf, "%g z", m_project[0][2]);
      strcat(m_xLabel, buf);
    }
  }

  // Unit
  strcat(m_xLabel, " [cm]");

  // Initialisation of the y-axis label
  strcpy(m_yLabel, "\0");

  // x portion
  if (fabs(m_project[1][0] - 1) < tol) {
    strcat(m_yLabel, "x");
  } else if (fabs(m_project[1][0] + 1) < tol) {
    strcat(m_yLabel, "-x");
  } else if (m_project[1][0] > tol) {
    sprintf(buf, "%g x", m_project[1][0]);
    strcat(m_yLabel, buf);
  } else if (m_project[1][0] < -tol) {
    sprintf(buf, "%g x", m_project[1][0]);
    strcat(m_yLabel, buf);
  }

  // y portion
  if (strlen(m_yLabel) > 0) {
    if (m_project[1][1] < -tol) {
      strcat(m_yLabel, " - ");
    } else if (m_project[1][1] > tol) {
      strcat(m_yLabel, " + ");
    }
    if (fabs(m_project[1][1] - 1) < tol || 
        fabs(m_project[1][1] + 1) < tol) {
      strcat(m_yLabel, "y");
    } else if (fabs(m_project[1][1]) > tol) {
      sprintf(buf, "%g y", fabs(m_project[1][1]));
      strcat(m_yLabel, buf);
    }
  } else {
    if (fabs(m_project[1][1] - 1) < tol) {
      strcat(m_yLabel, "y");
    } else if (fabs(m_project[1][1] + 1) < tol) {
      strcat(m_yLabel, "-y");
    } else if (m_project[1][1] > tol) {
      sprintf(buf, "%g y", m_project[1][1]);
      strcat(m_yLabel, buf);
    } else if (m_project[1][1] < -tol) {
      sprintf(buf, "%g y", m_project[1][1]);
      strcat(m_yLabel, buf);
    }
  }

  // z portion
  if (strlen(m_yLabel) > 0) {
    if (m_project[1][2] < -tol) {
      strcat(m_yLabel, " - ");
    } else if (m_project[1][2] > tol) {
      strcat(m_yLabel, " + ");
    }
    if (fabs(m_project[1][2] - 1) < tol || fabs(m_project[1][2] + 1) < tol) {
      strcat(m_yLabel, "z");
    } else if (fabs(m_project[1][2]) > tol) {
      sprintf(buf, "%g z", fabs(m_project[1][2]));
      strcat(m_yLabel, buf);
    }
  } else {
    if (fabs(m_project[1][2] - 1) < tol) {
      strcat(m_yLabel, "z");
    } else if (fabs(m_project[1][2] + 1) < tol) {
      strcat(m_yLabel, "-z");
    } else if (m_project[1][2] > tol) {
      sprintf(buf, "%g z", m_project[1][2]);
      strcat(m_yLabel, buf);
    } else if (m_project[1][2] < -tol) {
      sprintf(buf, "%g z", m_project[1][2]);
      strcat(m_yLabel, buf);
    }
  }

  // Unit
  strcat(m_yLabel, " [cm]");

  // Initialisation of the plane label
  strcpy(m_description, "\0");

  // x portion
  if (fabs(m_plane[0] - 1) < tol) {
    strcat(m_description, "x");
  } else if (fabs(m_plane[0] + 1) < tol) {
    strcat(m_description, "-x");
  } else if (m_plane[0] > tol) {
    sprintf(buf, "%g x", m_plane[0]);
    strcat(m_description, buf);
  } else if (m_plane[0] < -tol) {
    sprintf(buf, "%g x", m_plane[0]);
    strcat(m_description, buf);
  }

  // y portion
  if (strlen(m_description) > 0) {
    if (m_plane[1] < -tol) {
      strcat(m_description, " - ");
    } else if (m_plane[1] > tol) {
      strcat(m_description, " + ");
    }
    if (fabs(m_plane[1] - 1) < tol || fabs(m_plane[1] + 1) < tol) {
      strcat(m_description, "y");
    } else if (fabs(m_plane[1]) > tol) {
      sprintf(buf, "%g y", fabs(m_plane[1]));
      strcat(m_description, buf);
    }
  } else {
    if (fabs(m_plane[1] - 1) < tol) {
      strcat(m_description, "y");
    } else if (fabs(m_plane[1] + 1) < tol) {
      strcat(m_description, "-y");
    } else if (m_plane[1] > tol) {
      sprintf(buf, "%g y", m_plane[1]);
      strcat(m_description, buf);
    } else if (m_plane[1] < -tol) {
      sprintf(buf, "%g y", m_plane[1]);
      strcat(m_description, buf);
    }
  }

  // z portion
  if (strlen(m_description) > 0) {
    if (m_plane[2] < -tol) {
      strcat(m_description, " - ");
    } else if (m_plane[2] > tol) {
      strcat(m_description, " + ");
    }
    if (fabs(m_plane[2] - 1) < tol || fabs(m_plane[2] + 1) < tol) {
      strcat(m_description, "z");
    } else if (fabs(m_plane[2]) > tol) {
      sprintf(buf, "%g z", fabs(m_plane[2]));
      strcat(m_description, buf);
    }
  } else {
    if (fabs(m_plane[2] - 1) < tol) {
      strcat(m_description, "z");
    } else if (fabs(m_plane[2] + 1) < tol) {
      strcat(m_description, "-z");
    } else if (m_plane[2] > tol) {
      sprintf(buf, "%g z", m_plane[2]);
      strcat(m_description, buf);
    } else if (m_plane[2] < -tol) {
      sprintf(buf, "%g z", m_plane[2]);
      strcat(m_description, buf);
    }
  }

  // Constant
  sprintf(buf, " = %g", m_plane[3]);
  strcat(m_description, buf);

  if (m_debug) {
    std::cout << m_className << "::Labels:\n"
              << "    x label: |" << m_xLabel << "|\n"
              << "    y label: |" << m_yLabel << "|\n"
              << "    plane:   |" << m_description << "|\n";
  }
}

void ViewField::SetPlane(const double fx, const double fy, const double fz,
                         const double x0, const double y0, const double z0) {

  // Calculate two in-plane vectors for the normal vector
  const double fnorm = sqrt(fx * fx + fy * fy + fz * fz);
  if (fnorm > 0 && fx * fx + fz * fz > 0) {
    const double fxz = sqrt(fx * fx + fz * fz);
    m_project[0][0] = fz / fxz;
    m_project[0][1] = 0;
    m_project[0][2] = -fx / fxz;
    m_project[1][0] = -fx * fy / (fxz * fnorm);
    m_project[1][1] = (fx * fx + fz * fz) / (fxz * fnorm);
    m_project[1][2] = -fy * fz / (fxz * fnorm);
    m_project[2][0] = x0;
    m_project[2][1] = y0;
    m_project[2][2] = z0;
  } else if (fnorm > 0 && fy * fy + fz * fz > 0) {
    const double fyz = sqrt(fy * fy + fz * fz);
    m_project[0][0] = (fy * fy + fz * fz) / (fyz * fnorm);
    m_project[0][1] = -fx * fz / (fyz * fnorm);
    m_project[0][2] = -fy * fz / (fyz * fnorm);
    m_project[1][0] = 0;
    m_project[1][1] = fz / fyz;
    m_project[1][2] = -fy / fyz;
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
    m_hasExternalCanvas = false;
  }
  m_canvas->cd();
}

ViewField::PlotType ViewField::GetPlotType(const std::string& option,
                                           std::string& title) const {

  if (option == "v" || option == "p" || option == "phi" || option == "volt" ||
      option == "voltage" || option == "pot" || option == "potential") {
    title = "potential";
    return Potential;
  } else if (option == "e" || option == "field") {
    title = "field";
    return Magnitude;
  } else if (option == "ex") {
    title = "field (x-component)";
    return Ex;
  } else if (option == "ey") {
    title = "field (y-component)";
    return Ey;
  } else if (option == "ez") {
    title = "field (z-component)";
    return Ez; 
  } 
  std::cerr << m_className << "::GetPlotType:\n    Unknown option ("
            << option << ").\n";
  title = "potential";
  return Potential;
}

bool ViewField::SetupFunction(const std::string& option, TF2*& f, 
                              const bool contour, const bool wfield) {

  if (!m_sensor && !m_component) {
    std::cerr << m_className << "::SetupFunction:\n"
              << "    Neither sensor nor component are defined.\n";
    return false;
  }

  // Determine the x-y range (unless specified explicitly by the user).
  if (!m_hasUserArea) {
    // Try to get the area/bounding box from the sensor/component.
    double bbmin[3];
    double bbmax[3];
    if (m_sensor) {
      if (!m_sensor->GetArea(bbmin[0], bbmin[1], bbmin[2], 
                             bbmax[0], bbmax[1], bbmax[2])) {
        std::cerr << m_className << "::SetupFunction:\n"
                  << "    Sensor area is not defined.\n"
                  << "    Please set the plot range explicitly (SetArea).\n";
        return false;
      }
    } else {
      if (!m_component->GetBoundingBox(bbmin[0], bbmin[1], bbmin[2], 
                                       bbmax[0], bbmax[1], bbmax[2])) {
        std::cerr << m_className << "::SetupFunction:\n"
                  << "    Bounding box of the component is not defined.\n"
                  << "    Please set the plot range explicitly (SetArea).\n";
        return false;
      }
    }
    const double tol = 1.e-4;
    double umin[2] = {-std::numeric_limits<double>::max(),
                      -std::numeric_limits<double>::max()};
    double umax[2] = {std::numeric_limits<double>::max(),
                      std::numeric_limits<double>::max()};
    for (unsigned int i = 0; i < 3; ++i) {
      bbmin[i] -= m_project[2][i];
      bbmax[i] -= m_project[2][i];
      for (unsigned int j = 0; j < 2; ++j) {
        if (fabs(m_project[j][i]) < tol) continue;
        const double t1 = bbmin[i] / m_project[j][i];
        const double t2 = bbmax[i] / m_project[j][i];
        const double tmin = std::min(t1, t2); 
        const double tmax = std::max(t1, t2);
        if (tmin > umin[j] && tmin < umax[j]) umin[j] = tmin;
        if (tmax < umax[j] && tmax > umin[j]) umax[j] = tmax;
      }
    }
    m_xmin = umin[0];
    m_xmax = umax[0];
    m_ymin = umin[1];
    m_ymax = umax[1];
    std::cout << m_className << "::SetupFunction:\n     Setting plot range to "
              << m_xmin << " < x < " << m_xmax << ", " 
              << m_ymin << " < y < " << m_ymax << ".\n";
  }
  if (!f) {
    const std::string fname = FindUnusedFunctionName("f2D");
    f = new TF2(fname.c_str(), this, &ViewField::Evaluate2D, 
                m_xmin, m_xmax, m_ymin, m_ymax, 1, "ViewField", "Evaluate2D");
  }
  // Set the x-y range.
  f->SetRange(m_xmin, m_ymin, m_xmax, m_ymax);

  // Determine the quantity to be plotted.
  std::string title;
  const PlotType plotType = GetPlotType(option, title);
  const int parPlotType = 10 * int(plotType) + 1;

  // Set the z-range.
  double zmin = m_vmin;
  double zmax = m_vmax; 
  if (wfield) {
    title = "weighting " + title;
    f->SetParameter(0, -parPlotType);
    if (plotType == Potential) {
      zmin = 0.;
      zmax = 1.;
    } else if (m_useAutoRange) {
      SampleRange(m_xmin, m_ymin, m_xmax, m_ymax, f, zmin, zmax);
    } else {
      zmin = m_wmin;
      zmax = m_wmax;
    }
  } else {
    f->SetParameter(0, parPlotType);
    if (plotType == Potential) {
      if (m_useAutoRange) {
        if (m_component) {
          if (!m_component->GetVoltageRange(zmin, zmax)) {
            SampleRange(m_xmin, m_ymin, m_xmax, m_ymax, f, zmin, zmax);
          }
        } else if (m_sensor) {
          if (!m_sensor->GetVoltageRange(zmin, zmax)) {
            SampleRange(m_xmin, m_ymin, m_xmax, m_ymax, f, zmin, zmax);
          }
        }
      } else {
        zmin = m_vmin;
        zmax = m_vmax;
      }
    } else {
      title = "electric " + title;
      if (m_useAutoRange) {
        SampleRange(m_xmin, m_ymin, m_xmax, m_ymax, f, zmin, zmax);
      } else {
        zmin = m_emin;
        zmax = m_emax;
      }
    }
  }
  f->SetMinimum(zmin);
  f->SetMaximum(zmax);

  // Set the contours if requested.
  if (contour) {
    title = "Contours of the " + title;
    double level[m_nMaxContours];
    if (m_nContours > 1) {
      const double step = (zmax - zmin) / (m_nContours - 1.);
      for (unsigned int i = 0; i < m_nContours; ++i) {
        level[i] = zmin + i * step;
      } 
    } else {
     level[0] = 0.5 * (zmax + zmin);
    }
    if (m_debug) {
      std::cout << m_className << "::SetupFunction:\n"
                << "    Number of contours: " << m_nContours << "\n";
      for (unsigned int i = 0; i < m_nContours; ++i) {
        std::cout << "        Level " << i << " = " << level[i] << "\n";
      }
    }
    f->SetContour(m_nContours, level);
  }

  // Set the resolution.
  f->SetNpx(m_nSamples2dX);
  f->SetNpy(m_nSamples2dY);

  // Set the labels.
  f->GetXaxis()->SetTitle(m_xLabel);
  f->GetYaxis()->SetTitle(m_yLabel);
  f->SetTitle(title.c_str());

  return true;
}

bool ViewField::SetupProfile(const double x0, const double y0, const double z0,
                             const double x1, const double y1, const double z1,
                             const std::string& option, TF1*& f, 
                             const bool wfield) {

  if (!m_sensor && !m_component) {
    std::cerr << m_className << "::SetupProfile:\n"
              << "    Neither sensor nor component are defined.\n";
    return false;
  }

  // Check the distance between the two points.
  const double dx = x1 - x0;
  const double dy = y1 - y0;
  const double dz = z1 - z0;
  if (dx * dx + dy * dy + dz * dz <= 0.) {
    std::cerr << m_className << "::SetupProfile:\n"
              << "    Start and end points coincide.\n";
    return false;
  }

  if (!f) { 
    const std::string fname = FindUnusedFunctionName("fProfile");
    f = new TF1(fname.c_str(), this, &ViewField::EvaluateProfile, 
                0., 1., 7, "ViewField", "EvaluateProfile");
  }
  f->SetParameter(0, x0);
  f->SetParameter(1, y0);
  f->SetParameter(2, z0);
  f->SetParameter(3, x1);
  f->SetParameter(4, y1);
  f->SetParameter(5, z1);

  // Determine the quantity to be plotted.
  std::string title;
  const PlotType plotType = GetPlotType(option, title);
  const int parPlotType = 10 * int(plotType) + 1;

  double fmin = m_vmin;
  double fmax = m_vmax;
  if (wfield) {
    f->SetParameter(6, -parPlotType);
    title = "weighting " + title;
    if (plotType == Potential) {
      fmin = 0.;
      fmax = 1.;
    } else {
      if (m_useAutoRange) {
        SampleRange(f, fmin, fmax);
      } else {
        fmin = m_wmin;
        fmax = m_wmax;
      }
    }
  } else {
    f->SetParameter(6, parPlotType);
    if (plotType == Potential) {
      if (m_useAutoRange) {
        if (m_component) {
          if (!m_component->GetVoltageRange(fmin, fmax)) {
            SampleRange(f, fmin, fmax);
          }
        } else if (m_sensor) {
          if (!m_sensor->GetVoltageRange(fmin, fmax)) {
            SampleRange(f, fmin, fmax);
          }
        }
      } else {
        fmin = m_vmin;
        fmax = m_vmax;
      }
    } else {
      title = "electric " + title;
      if (m_useAutoRange) {
        SampleRange(f, fmin, fmax);
      } else {
        fmin = m_emin;
        fmax = m_emax;
      }
    }
  }
  f->SetMinimum(fmin);
  f->SetMaximum(fmax);
  if (m_debug) {
    std::cout << m_className << "::SetupProfile:\n"
              << "    Plotting " << title << " along\n    ("
              << f->GetParameter(0) << ", " << f->GetParameter(1) << ", "
              << f->GetParameter(2) << ") - ("
              << f->GetParameter(3) << ", " << f->GetParameter(4) << ", "
              << f->GetParameter(5) << ")\n";
  }

  title = "Profile plot of the " + title;
  f->SetTitle(title.c_str());
  f->GetXaxis()->SetTitle("normalised distance");
  if (plotType == Potential) {
    f->GetYaxis()->SetTitle("potential [V]");
  } else {
    f->GetYaxis()->SetTitle("field [V/cm]");
  }
  f->SetNpx(m_nSamples1d);
  return true;
}

}
