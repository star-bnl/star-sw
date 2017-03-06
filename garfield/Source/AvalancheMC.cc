#include <iostream>
#include <fstream>
#include <cmath>
#include <string>

#include "AvalancheMC.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"
#include "Random.hh"

namespace Garfield {

double AvalancheMC::c1 = ElectronMass / (SpeedOfLight * SpeedOfLight);

AvalancheMC::AvalancheMC()
    : m_sensor(NULL),
      m_stepModel(2),
      m_tMc(0.02),
      m_dMc(0.001),
      m_nMc(100),
      m_hasTimeWindow(false),
      m_tMin(0.),
      m_tMax(0.),
      m_nElectrons(0),
      m_nHoles(0),
      m_nIons(0),
      m_viewer(NULL),
      m_useSignal(false),
      m_useInducedCharge(false),
      m_useEquilibration(true),
      m_useDiffusion(true),
      m_useAttachment(true),
      m_useBfield(false),
      m_useIons(true),
      m_withElectrons(true),
      m_withHoles(true),
      m_scaleElectronSignal(1.),
      m_scaleHoleSignal(1.),
      m_scaleIonSignal(1.),
      m_useTcadTrapping(false),
      m_useTcadVelocity(false),
      m_debug(false) {

  m_className = "AvalancheMC";

  m_drift.reserve(10000);
}

void AvalancheMC::SetSensor(Sensor* sensor) {

  if (!sensor) {
    std::cerr << m_className << "::SetSensor:\n    Null pointer.\n";
    return;
  }

  m_sensor = sensor;
}

void AvalancheMC::EnablePlotting(ViewDrift* view) {

  if (!view) {
    std::cerr << m_className << "::EnablePlotting:\n    Null pointer.\n";
    return;
  }

  m_viewer = view;
}

void AvalancheMC::SetTimeSteps(const double d) {

  m_stepModel = 0;
  if (d < Small) {
    std::cerr << m_className << "::SetTimeSteps:\n    "
              << "Step size is too small. Using default (20 ps) instead.\n";
    m_tMc = 0.02;
    return;
  } 
  if (m_debug) {
    std::cout << m_className << "::SetTimeSteps:\n"
              << "    Step size set to " << d << " ns.\n";
  }
  m_tMc = d;
}

void AvalancheMC::SetDistanceSteps(const double d) {

  m_stepModel = 1;
  if (d < Small) {
    std::cerr << m_className << "::SetDistanceSteps:\n    "
              << "Step size is too small. Using default (10 um) instead.\n";
    m_dMc = 0.001;
    return;
  } 
  if (m_debug) {
    std::cout << m_className << "::SetDistanceSteps:\n"
              << "    Step size set to " << d << " cm.\n";
  }
  m_dMc = d;
}

void AvalancheMC::SetCollisionSteps(const int n) {

  m_stepModel = 2;
  if (n < 1) {
    std::cerr << m_className << "::SetCollisionSteps:\n    "
              << "Number of collisions set to default value (100).\n";
    m_nMc = 100;
    return;
  } 
  if (m_debug) {
    std::cout << m_className << "::SetCollisionSteps:\n    "
              << "Number of collisions to be skipped set to " << n << ".\n";
  }
  m_nMc = n;
}

void AvalancheMC::SetTimeWindow(const double t0, const double t1) {

  if (fabs(t1 - t0) < Small) {
    std::cerr << m_className << "::SetTimeWindow:\n"
              << "    Time interval must be greater than zero.\n";
    return;
  }

  m_tMin = std::min(t0, t1);
  m_tMax = std::max(t0, t1);
  m_hasTimeWindow = true;
}

void AvalancheMC::GetDriftLinePoint(const unsigned int i, double& x, double& y,
                                    double& z, double& t) const {

  if (i >= m_drift.size()) {
    std::cerr << m_className << "::GetDriftLinePoint:\n"
              << "    Drift line point " << i << " does not exist.\n";
    return;
  }

  x = m_drift[i].x;
  y = m_drift[i].y;
  z = m_drift[i].z;
  t = m_drift[i].t;
}

void AvalancheMC::GetHoleEndpoint(const unsigned int i, double& x0, double& y0,
                                  double& z0, double& t0, double& x1,
                                  double& y1, double& z1, double& t1,
                                  int& status) const {

  if (i >= m_endpointsHoles.size()) {
    std::cerr << m_className << "::GetHoleEndpoint:\n"
              << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = m_endpointsHoles[i].x0;
  y0 = m_endpointsHoles[i].y0;
  z0 = m_endpointsHoles[i].z0;
  t0 = m_endpointsHoles[i].t0;
  x1 = m_endpointsHoles[i].x1;
  y1 = m_endpointsHoles[i].y1;
  z1 = m_endpointsHoles[i].z1;
  t1 = m_endpointsHoles[i].t1;
  status = m_endpointsHoles[i].status;
}

void AvalancheMC::GetIonEndpoint(const unsigned int i, double& x0, double& y0,
                                 double& z0, double& t0, double& x1, double& y1,
                                 double& z1, double& t1, int& status) const {

  if (i >= m_endpointsIons.size()) {
    std::cerr << m_className << "::GetIonEndpoint:\n"
              << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = m_endpointsIons[i].x0;
  y0 = m_endpointsIons[i].y0;
  z0 = m_endpointsIons[i].z0;
  t0 = m_endpointsIons[i].t0;
  x1 = m_endpointsIons[i].x1;
  y1 = m_endpointsIons[i].y1;
  z1 = m_endpointsIons[i].z1;
  t1 = m_endpointsIons[i].t1;
  status = m_endpointsIons[i].status;
}

void AvalancheMC::GetElectronEndpoint(const unsigned int i, double& x0,
                                      double& y0, double& z0, double& t0,
                                      double& x1, double& y1, double& z1,
                                      double& t1, int& status) const {

  if (i >= m_endpointsElectrons.size()) {
    std::cerr << m_className << "::GetElectronEndpoint:\n"
              << "    Endpoint " << i << " does not exist.\n";
    return;
  }

  x0 = m_endpointsElectrons[i].x0;
  y0 = m_endpointsElectrons[i].y0;
  z0 = m_endpointsElectrons[i].z0;
  t0 = m_endpointsElectrons[i].t0;
  x1 = m_endpointsElectrons[i].x1;
  y1 = m_endpointsElectrons[i].y1;
  z1 = m_endpointsElectrons[i].z1;
  t1 = m_endpointsElectrons[i].t1;
  status = m_endpointsElectrons[i].status;
}

bool AvalancheMC::DriftElectron(const double x0, const double y0,
                                const double z0, const double t0) {

  if (!m_sensor) {
    std::cerr << m_className << "::DriftElectron:\n"
              << "    Sensor is not defined.\n";
    return false;
  }

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();

  m_nElectrons = 1;
  m_nHoles = 0;
  m_nIons = 0;

  return DriftLine(x0, y0, z0, t0, -1);
}

bool AvalancheMC::DriftHole(const double x0, const double y0, const double z0,
                            const double t0) {

  if (!m_sensor) {
    std::cerr << m_className << "::DriftHole:\n    Sensor is not defined.\n";
    return false;
  }

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();

  m_nElectrons = 0;
  m_nHoles = 1;
  m_nIons = 0;

  return DriftLine(x0, y0, z0, t0, 1);
}

bool AvalancheMC::DriftIon(const double x0, const double y0, const double z0,
                           const double t0) {

  if (!m_sensor) {
    std::cerr << m_className << "::DriftIon:\n    Sensor is not defined.\n";
    return false;
  }

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();

  m_nElectrons = 0;
  m_nHoles = 0;
  m_nIons = 1;

  return DriftLine(x0, y0, z0, t0, 2);
}

bool AvalancheMC::DriftLine(const double x0, const double y0, const double z0,
                            const double t0, const int type, const bool aval) {

  const std::string hdr = m_className + "::DriftLine:\n    ";
  // Reset the drift line.
  m_drift.clear();
  // Current position
  double x = x0, y = y0, z = z0;
  // Current time.
  double t = t0;
  // Current drift point.
  driftPoint point;
  point.x = x0;
  point.y = y0;
  point.z = z0;
  point.t = t0;
  point.ne = 0;
  point.nh = 0;
  point.ni = 0;

  int status = 0;
  while (0 == status) {
    // Get the electric and magnetic field at the current position.
    double ex = 0., ey = 0., ez = 0.;
    double bx = 0., by = 0., bz = 0.;
    Medium* medium = NULL;
    status = GetField(x, y, z, ex, ey, ez, bx, by, bz, medium);
    if (status == StatusCalculationAbandoned) {
      // Something went wrong.
      std::cerr << hdr << "Abandoning the calculation.\n";
    } else if (status == StatusLeftDriftMedium || medium == NULL) {
      // Point is not inside a "driftable" medium.
      if (m_drift.empty()) {
        std::cerr << hdr << "Initial position (" << x << ", " << y << ", " 
                  << z << ") is not inside a drift medium.\n";
      } else { 
        // Try terminating the drift line close to the boundary.
        TerminateLine(point.x, point.y, point.z, point.t, x, y, z, t);
        if (m_debug) {
          std::cout << hdr << "Particle left the drift medium at ("
                    << x << ", " << y << ", " << z << ").\n";
        }
      }
    } else if (!m_sensor->IsInArea(x, y, z)) {
      // Point is not inside the drift area of the sensor.
      status = StatusLeftDriftArea;
      if (m_drift.empty()) {
        std::cerr << hdr << "Initial position (" << x << ", " << y << ", " 
                  << z << ") is not inside the drift area.\n";
      } else {
        // Try terminating the drift line close to the boundary.
        TerminateLine(point.x, point.y, point.z, point.t, x, y, z, t);
        if (m_debug) {
          std::cout << hdr << "Particle left the drift area at ("
                    << x << ", " << y << ", " << z << ").\n";
        }
      }
    } else if (!m_drift.empty()) {
      // Check if the particle has crossed a wire.
      double xc = point.x;
      double yc = point.y;
      double zc = point.z;
      if (m_sensor->IsWireCrossed(xc, yc, zc, x, y, z, xc, yc, zc)) {
        status = StatusLeftDriftMedium;
        // Update the position and time.
        x = xc;
        y = yc;
        z = zc;
        // Adjust the time step.
        const double dxc = xc - point.x;
        const double dyc = yc - point.y;
        const double dzc = zc - point.z;
        const double dxp = x - point.x;
        const double dyp = y - point.y;
        const double dzp = z - point.z;
        const double dsc = sqrt(dxc * dxc + dyc * dyc + dzc * dzc);
        const double dsp = sqrt(dxp * dxp + dyp * dyp + dzp * dzp);
        t = point.t + (t - point.t) * dsc / dsp;
        if (m_debug) {
          std::cout << hdr << "Particle hit a wire at ("
                    << xc << ", " << yc << ", " << zc << ").\n";
        }
      }
    }

    // Make sure the time is still within the specified interval.
    if (m_hasTimeWindow && (t < m_tMin || t > m_tMax)) {
      status = StatusOutsideTimeWindow;
      if (m_drift.empty()) {
        std::cerr << hdr << "Initial time (" << t0 << ") is not inside the "
                  << "time window (" << m_tMin << ", " << m_tMax << ").\n";
      } 
    }

    // Add the point to the drift line.
    point.x = x;
    point.y = y;
    point.z = z;
    point.t = t;
    m_drift.push_back(point);

    // Stop if the drift line has ended.
    if (status != 0) break;

    // Make sure the electric field has a non-vanishing component.
    const double emag = sqrt(ex * ex + ey * ey + ez * ez);
    if (emag < Small) {
      std::cerr << hdr << "Electric field at (" << x << ", " << y << ", " << z 
                << ") is too small.\n";
      status = StatusCalculationAbandoned;
      break;
    }
    // Compute the drift velocity at this point.
    double vx = 0., vy = 0., vz = 0.;
    if (!GetVelocity(type, medium, x, y, z, ex, ey, ez, bx, by, bz, 
                     vx, vy, vz)) {
      status = StatusCalculationAbandoned;
      std::cerr << hdr << "Abandoning the calculation.\n";
      break;
    }
    // Make sure the drift velocity vector has a non-vanishing component.
    const double vmag = sqrt(vx * vx + vy * vy + vz * vz);
    if (vmag < Small) {
      std::cerr << hdr << "Drift velocity at (" << x << ", " << y << ", " << z
                << ") is too small.\n";
      status = StatusCalculationAbandoned;
      break;
    }

    // Determine the time step.
    double dt = 0.;
    switch (m_stepModel) {
      case 0:
        // Fixed time steps
        dt = m_tMc;
        break;
      case 1:
        // Fixed distance steps
        dt = m_dMc / vmag;
        break;
      case 2:
        // Steps based on collision time
        dt = -m_nMc * (c1 * vmag / emag) * log(RndmUniformPos());
        break;
      default:
        std::cerr << hdr << "Unknown stepping model. Program bug!\n";
        status = StatusCalculationAbandoned;
        return false;
    }

    // Compute the proposed end-point of this step.
    x += dt * vx;
    y += dt * vy;
    z += dt * vz;
    t += dt;
    if (m_useDiffusion) {
      if (!AddDiffusion(type, medium, sqrt(vmag * dt), x, y, z, vx, vy, vz,
                        ex, ey, ez, bx, by, bz)) {
        status = StatusCalculationAbandoned;
        std::cerr << hdr << "Abandoning the calculation.\n";
        break;
      }
    } 
    if (m_debug) {
      std::cout << hdr << "New point: " << x << ", " << y << ", " << z << "\n";
    }
  }

  // Compute Townsend and attachment coefficients for each drift step.
  unsigned int nElectronsOld = m_nElectrons;
  unsigned int nHolesOld = m_nHoles;
  unsigned int nIonsOld = m_nIons;

  if ((type == -1 || type == 1) && (aval || m_useAttachment)) {
    ComputeGainLoss(type, status);
    if (status == StatusAttached && m_debug) {
      std::cout << hdr << "Particle attached at (" << m_drift.back().x << ", "
                << m_drift.back().y << ", " << m_drift.back().z << "\n";
    }
  }

  // Create an "endpoint".
  endpoint endPoint;
  endPoint.x0 = x0;
  endPoint.y0 = y0;
  endPoint.z0 = z0;
  endPoint.t0 = t0;
  endPoint.x1 = m_drift.back().x;
  endPoint.y1 = m_drift.back().y;
  endPoint.z1 = m_drift.back().z;
  endPoint.t1 = m_drift.back().t;
  endPoint.status = status;
  if (type == -1) {
    m_endpointsElectrons.push_back(endPoint);
  } else if (type == 1) {
    m_endpointsHoles.push_back(endPoint);
  } else if (type == 2) {
    m_endpointsIons.push_back(endPoint);
  }
  if (m_debug) {
    const int nNewElectrons = m_nElectrons - nElectronsOld;
    const int nNewHoles = m_nHoles - nHolesOld;
    const int nNewIons = m_nIons - nIonsOld;
    std::cout << hdr << "Produced\n"
              << "      " << nNewElectrons << " electrons,\n"
              << "      " << nNewHoles << " holes, and\n"
              << "      " << nNewIons << " ions\n"
              << "    along the drift line from \n"
              << "      (" << endPoint.x0 << ", " << endPoint.y0 << ", "
              << endPoint.z0 << ") to \n"
              << "      (" << endPoint.x1 << ", " << endPoint.y1 << ", "
              << endPoint.z1 << ").\n";
  }

  // Compute the induced signal and induced charge if requested.
  const double scale = type == -1 ? -m_scaleElectronSignal : 
                       type ==  1 ?  m_scaleHoleSignal : m_scaleIonSignal; 
  if (m_useSignal) ComputeSignal(scale);
  if (m_useInducedCharge) ComputeInducedCharge(scale);

  // Plot the drift line if requested.
  if (m_viewer && !m_drift.empty()) {
    const unsigned int nPoints = m_drift.size();
    // Register the new drift line and get its ID.
    int id;
    if (type < 0) {
      m_viewer->NewElectronDriftLine(nPoints, id, x0, y0, z0);
    } else if (type == 1) {
      m_viewer->NewHoleDriftLine(nPoints, id, x0, y0, z0);
    } else {
      m_viewer->NewIonDriftLine(nPoints, id, x0, y0, z0);
    }
    // Set the points along the trajectory.
    for (unsigned int i = 0; i < nPoints; ++i) {
      m_viewer->SetDriftLinePoint(id, i, m_drift[i].x, m_drift[i].y,
                                  m_drift[i].z);
    }
  }

  if (status == StatusCalculationAbandoned) return false;
  return true;
}

bool AvalancheMC::AvalancheElectron(const double x0, const double y0,
                                    const double z0, const double t0,
                                    const bool holes) {

  m_withHoles = holes;
  return Avalanche(x0, y0, z0, t0, 1, 0, 0);
}

bool AvalancheMC::AvalancheHole(const double x0, const double y0,
                                const double z0, const double t0,
                                const bool electrons) {

  m_withElectrons = electrons;
  return Avalanche(x0, y0, z0, t0, 0, 1, 0);
}

bool AvalancheMC::AvalancheElectronHole(const double x0, const double y0,
                                        const double z0, const double t0) {

  m_withElectrons = m_withHoles = true;
  return Avalanche(x0, y0, z0, t0, 1, 1, 0);
}

bool AvalancheMC::Avalanche(const double x0, const double y0, const double z0,
                            const double t0,
                            const unsigned int ne0, const unsigned int nh0,
                            const unsigned int ni0) {

  const std::string hdr = m_className + "::Avalanche:\n    ";

  m_endpointsElectrons.clear();
  m_endpointsHoles.clear();
  m_endpointsIons.clear();

  // Make sure the sensor is defined.
  if (!m_sensor) {
    std::cerr << hdr << "Sensor is not defined.\n";
    return false;
  }

  // Add the first point to the list.
  struct avalPoint {
    double x, y, z, t;
    int ne, nh, ni;
  };
  std::vector<avalPoint> aval;
  avalPoint point;
  point.x = x0;
  point.y = y0;
  point.z = z0;
  point.t = t0;
  point.ne = ne0;
  point.nh = nh0;
  point.ni = ni0;
  aval.push_back(point);

  m_nElectrons = ne0;
  m_nHoles = nh0;
  m_nIons = ni0;

  if (!m_withHoles && !m_withElectrons) {
    std::cerr << hdr << "Neither electron nor hole/ion component requested.\n";
  }

  std::vector<avalPoint> newAval;
  while (!aval.empty()) {
    std::vector<avalPoint>::iterator it;
    std::vector<avalPoint>::iterator end = aval.end(); 
    for (it = aval.begin(); it != end; ++it) {
      if (m_withElectrons) {
        // Loop over the electrons at this location.
        const unsigned int ne = (*it).ne;
        for (unsigned int i = 0; i < ne; ++i) {
          // Compute an electron drift line.
          if (!DriftLine((*it).x, (*it).y, (*it).z, (*it).t, -1, true)) {
            continue;
          }
          // Loop over the drift line.
          const unsigned int nPoints = m_drift.size();
          // TODO: why - 2?
          for (unsigned int j = 0; j < nPoints - 2; ++j) {
            if (m_drift[j].ne > 0 || m_drift[j].nh > 0 || m_drift[j].ni > 0) {
              // Add the point to the table.
              point.x = m_drift[j + 1].x;
              point.y = m_drift[j + 1].y;
              point.z = m_drift[j + 1].z;
              point.t = m_drift[j + 1].t;
              point.ne = m_drift[j].ne;
              point.nh = m_drift[j].nh;
              point.ni = m_drift[j].ni;
              newAval.push_back(point);
            }
          }
        }
      }

      if (m_withHoles) {
        // Loop over the ions at this location.
        const unsigned int ni = (*it).ni;
        for (unsigned int i = 0; i < ni; ++i) {
          // Compute an ion drift line.
          DriftLine((*it).x, (*it).y, (*it).z, (*it).t, 2, false);
        }

        // Loop over the holes at this location.
        const unsigned int nh = (*it).nh;
        for (unsigned int i = 0; i < nh; ++i) {
          // Compute a hole drift line.
          if (!DriftLine((*it).x, (*it).y, (*it).z, (*it).t, +1, true)) {
            continue;
          }
          // Loop over the drift line.
          const unsigned int nPoints = m_drift.size();
          for (unsigned int j = 0; j < nPoints - 1; ++j) {
            if (m_drift[j].ne > 0 || m_drift[j].nh > 0 || m_drift[j].ni > 0) {
              // Add the point to the table.
              point.x = m_drift[j + 1].x;
              point.y = m_drift[j + 1].y;
              point.z = m_drift[j + 1].z;
              point.t = m_drift[j + 1].t;
              point.ne = m_drift[j].ne;
              point.nh = m_drift[j].nh;
              point.ni = m_drift[j].ni;
              newAval.push_back(point);
            }
          }
        }
      }
    }
    aval.swap(newAval);
    newAval.clear();
  }
  return true;
}

int AvalancheMC::GetField(const double x, const double y, const double z,
                          double& ex, double& ey, double& ez,
                          double& bx, double& by, double& bz,
                          Medium*& medium) {

  // Get the electric field.
  int status = 0;
  m_sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
  // Make sure the point is inside a drift medium.
  if (status != 0) return StatusLeftDriftMedium;
 
  // Get the magnetic field, if requested.
  if (m_useBfield) {
    m_sensor->MagneticField(x, y, z, bx, by, bz, status);
    bx *= Tesla2Internal;
    by *= Tesla2Internal;
    bz *= Tesla2Internal;
  }
  return 0;
}

bool AvalancheMC::GetVelocity(const int type, Medium* medium, 
                              const double x, const double y, const double z,
                              const double ex, const double ey, const double ez,
                              const double bx, const double by, const double bz,
                              double& vx, double& vy, double& vz) {

  if (type != -1 && type != 1 && type != 2) {
    std::cerr << m_className << "::GetVelocity:\n    "
              << "Unknown drift line type (" << type << "). Program bug!\n";
    return false;
  }
  if (m_useTcadVelocity && type != 2) {
    // We assume there is only one component with active velocity.
    const unsigned int nComponents = m_sensor->GetNumberOfComponents();
    for (unsigned int i = 0; i < nComponents; ++i) {
      ComponentBase* cmp = m_sensor->GetComponent(i);
      if (!cmp->IsVelocityActive()) continue;
      Medium* m = NULL;
      int status = 0;
      if (type < 0) {
        cmp->ElectronVelocity(x, y, z, vx, vy, vz, m, status);
      } else if (type == 1) {
        cmp->HoleVelocity(x, y, z, vx, vy, vz, m, status);
      } 
      if (status != 0) {
        const std::string eh = type < 0 ? "electron" : "hole";
        std::cerr << m_className << "::GetVelocity:\n    "
                  << "Error calculating " << eh << " TCAD velocity at ("
                  << x << ", " << y << ", " << z << ")\n";
        return false;
      }
      // Seems to have worked.
      if (m_debug) {
        std::cout << m_className << "::GetVelocity:\n    "
                  << "TCAD drift velocity at (" << x << ", " << y << ", " << z
                  << "): " << vx << ", " << vy << ", " << vz << "\n";
      }
      return true;
    }
  }
  bool ok = false;
  if (type < 0) {
    ok = medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  } else if (type == 1) {
    ok = medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  } else if (type == 2) {
    ok = medium->IonVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
  } 
  if (!ok) {
    const std::string ehi = type < 0 ? "electron" : type == 1 ? "hole" : "ion";
    std::cerr << m_className << "::GetVelocity:\n    Error calculating " << ehi
              << " velocity at (" << x << ", " << y << ", " << z << ")\n";
    return false;
  }
  if (m_debug) {
    std::cout << m_className << "::GetVelocity:\n    "
              << "Drift velocity at (" << x << ", " << y << ", " << z << "): " 
              << vx << ", " << vy << ", " << vz << "\n";
  }
  return true;
}

bool AvalancheMC::AddDiffusion(const int type, Medium* medium, 
    const double step, double& x, double& y, double& z, 
    const double vx, const double vy, const double vz,
    const double ex, const double ey, const double ez,
    const double bx, const double by, const double bz) {

  bool ok = false;
  double dl = 0., dt = 0.;
  if (type < 0) {
    ok = medium->ElectronDiffusion(ex, ey, ez, bx, by, bz, dl, dt);
  } else if (type == 1) {
    ok = medium->HoleDiffusion(ex, ey, ez, bx, by, bz, dl, dt);
  } else if (type == 2) {
    ok = medium->IonDiffusion(ex, ey, ez, bx, by, bz, dl, dt);
  }
  if (!ok) {
    const std::string ehi = type < 0 ? "electron" : type == 1 ? "hole" : "ion";
    std::cerr << m_className << "::AddDiffusion:\n    Error calculating " << ehi
              << " diffusion at (" << x << ", " << y << ", " << z << ")\n";
    return false;
  }

  // Draw a random diffusion direction in the particle frame.
  const double dx = step * RndmGaussian(0., dl);
  const double dy = step * RndmGaussian(0., dt);
  const double dz = step * RndmGaussian(0., dt);
  if (m_debug) {
    std::cout << m_className << "::AddDiffusion:\n    Adding diffusion step "
              << dx << ", " << dy << ", " << dz << "\n";
  }
  // Compute the rotation angles to align diffusion and drift velocity vectors.
  const double vt = sqrt(vx * vx + vy * vy);
  const double phi = vt > Small ? atan2(vy, vx) : 0.;
  const double theta = vt > Small ? atan2(vz, vt) : vz < 0. ? -HalfPi : HalfPi;
  const double cphi = cos(phi);
  const double sphi = sin(phi);
  const double ctheta = cos(theta);
  const double stheta = sin(theta);

  x += cphi * ctheta * dx - sphi * dy - cphi * stheta * dz;
  y += sphi * ctheta * dx + cphi * dy - sphi * stheta * dz;
  z += stheta * dx + ctheta * dz;
  return true;
}

void AvalancheMC::TerminateLine(double x0, double y0, double z0, double t0, 
                                double& x, double& y, double& z, double& t) {

  double dt = t - t0;
  double dx = x - x0;
  double dy = y - y0;
  double dz = z - z0;
  double ds = sqrt(dx * dx + dy * dy + dz * dz);
  if (ds > 0.) {
    dx /= ds;
    dy /= ds;
    dz /= ds;
  }
  while (ds > BoundaryDistance) {
    dt *= 0.5;
    ds *= 0.5;
    const double xm = x0 + dx * ds;
    const double ym = y0 + dy * ds;
    const double zm = z0 + dz * ds;
    // Check if the mid-point is inside the drift medium and the drift area.
    double ex = 0., ey = 0., ez = 0.;
    int status = 0;
    Medium* medium = NULL;
    m_sensor->ElectricField(xm, ym, zm, ex, ey, ez, medium, status);
    if (status == 0 && m_sensor->IsInArea(xm, ym, zm)) {
      x0 = xm;
      y0 = ym;
      z0 = zm;
      t0 += dt;
    }
  }
  // Place the particle OUTSIDE the drift medium and/or the drift area.
  x = x0 + dx * ds;
  y = y0 + dy * ds;
  z = z0 + dz * ds;
  t = t0 + dt;
}

bool AvalancheMC::ComputeGainLoss(const int type, int& status) {

  const unsigned int nPoints = m_drift.size();
  std::vector<double> alphas(nPoints, 0.);
  std::vector<double> etas(nPoints, 0.);
  // Compute the integrated Townsend and attachment coefficients.
  if (!ComputeAlphaEta(type, alphas, etas)) return false;

  // Subdivision of a step
  const double probth = 0.01;

  // Set initial number of electrons/ions.
  int ne = 1, ni = 0;
  // Loop over the drift line.
  for (unsigned int i = 0; i < nPoints - 1; ++i) {
    m_drift[i].ne = 0;
    m_drift[i].nh = 0;
    m_drift[i].ni = 0;
    // Compute the number of subdivisions.
    const int nDiv = std::max(int((alphas[i] + etas[i]) / probth), 1);
    // Compute the probabilities for gain and loss.
    const double palpha = std::max(alphas[i] / nDiv, 0.);
    const double peta = std::max(etas[i] / nDiv, 0.);
    // Set initial number of electrons/ions.
    int neInit = ne;
    int niInit = ni;
    // Loop over the subdivisions.
    for (int j = 0; j < nDiv; ++j) {
      if (ne > 100) {
        // Gaussian approximation.
        const int gain = int(
            ne * palpha + RndmGaussian() * sqrt(ne * palpha * (1. - palpha)));
        const int loss =
            int(ne * peta + RndmGaussian() * sqrt(ne * peta * (1. - peta)));
        ne += gain - loss;
        ni += gain;
      } else {
        // Binomial approximation
        for (int k = ne; k--;) {
          if (RndmUniform() < palpha) {
            ++ne;
            ++ni;
          }
          if (RndmUniform() < peta) --ne;
        }
      }
      // Check if the particle has survived.
      if (ne <= 0) {
        status = StatusAttached;
        if (type == -1) {
          --m_nElectrons;
        } else if (type == 1) {
          --m_nHoles;
        } else {
          --m_nIons;
         }
        m_drift.resize(i + 2);
        m_drift[i + 1].x = 0.5 * (m_drift[i].x + m_drift[i + 1].x);
        m_drift[i + 1].y = 0.5 * (m_drift[i].y + m_drift[i + 1].y);
        m_drift[i + 1].z = 0.5 * (m_drift[i].z + m_drift[i + 1].z);
        break;
      }
    }
    // If at least one new electron has been created,
    // add the new electrons to the table.
    if (ne - neInit >= 1) {
      if (type == -1) {
        m_drift[i].ne = ne - neInit;
        m_nElectrons += ne - neInit;
      } else if (type == 1) {
        m_drift[i].nh = ne - neInit;
        m_nHoles += ne - neInit;
      } else {
        m_drift[i].ni = ne - neInit;
      }
    }
    if (ni - niInit >= 1) {
      if (type == -1) {
        if (m_useIons) {
          m_drift[i].ni = ni - niInit;
          m_nIons += ni - niInit;
        } else {
          m_drift[i].nh = ni - niInit;
          m_nHoles += ni - niInit;
        }
      } else {
        m_drift[i].ne = ni - niInit;
        m_nElectrons += ni - niInit;
      }
    }
    // If trapped, exit the loop over the drift line.
    if (status == StatusAttached) return true;
  }
  return true;
}

bool AvalancheMC::ComputeAlphaEta(const int type, std::vector<double>& alphas,
                                  std::vector<double>& etas) {
  // Locations and weights for 6-point Gaussian integration
  const double tg[6] = {-0.932469514203152028, -0.661209386466264514,
                        -0.238619186083196909,  0.238619186083196909,
                         0.661209386466264514,  0.932469514203152028};
  const double wg[6] = {0.171324492379170345, 0.360761573048138608,
                        0.467913934572691047, 0.467913934572691047,
                        0.360761573048138608, 0.171324492379170345};

  const unsigned int nPoints = m_drift.size();
  alphas.resize(nPoints, 0.);
  etas.resize(nPoints, 0.);
  if (nPoints < 2) return true;
  // Loop over the drift line.
  for (unsigned int i = 0; i < nPoints - 1; ++i) {
    // Compute the step length.
    const double delx = m_drift[i + 1].x - m_drift[i].x;
    const double dely = m_drift[i + 1].y - m_drift[i].y;
    const double delz = m_drift[i + 1].z - m_drift[i].z;
    const double del = sqrt(delx * delx + dely * dely + delz * delz);
    // Integrate drift velocity and Townsend and attachment coefficients.
    double vdx = 0.;
    double vdy = 0.;
    double vdz = 0.;
    alphas[i] = 0.;
    etas[i] = 0.;
    for (unsigned int j = 0; j < 6; ++j) {
      const double x = m_drift[i].x + 0.5 * (1. + tg[j]) * delx;
      const double y = m_drift[i].y + 0.5 * (1. + tg[j]) * dely;
      const double z = m_drift[i].z + 0.5 * (1. + tg[j]) * delz;
      // Get the electric field.
      double ex = 0., ey = 0., ez = 0.;
      Medium* medium = NULL;
      int status = 0;
      m_sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
      // Make sure that we are in a drift medium.
      if (status != 0) {
        // Check if this point is the last but one.
        if (i < nPoints - 2) {
          std::cerr << m_className << "::ComputeAlphaEta:    Got status \n"
                    << status << " at segment " << j + 1 
                    << "/6, drift point " << i + 1 << "/" << nPoints << ".\n";
          return false;
        }
        continue;
      }
      // Get the magnetic field.
      double bx = 0., by = 0., bz = 0.;
      if (m_useBfield) {
        m_sensor->MagneticField(x, y, z, bx, by, bz, status);
        bx *= Tesla2Internal;
        by *= Tesla2Internal;
        bz *= Tesla2Internal;
      }
      // Get drift velocity, Townsend and attachment coefficients.
      double vx = 0., vy = 0., vz = 0.;
      double alpha = 0., eta = 0.;
      if (m_useTcadTrapping) {
        // Only one component with active traps and velocity map is assumed to
        // be attached to AvalancheMC.
        const unsigned int nComponents = m_sensor->GetNumberOfComponents();
        for (unsigned int i = 0; i < nComponents; ++i) {
          ComponentBase* trapCmp = m_sensor->GetComponent(i);
          if (!trapCmp->IsTrapActive()) continue;
          Medium* trapMed = trapCmp->GetMedium(x, y, z);
          if (type < 0) {
            if (trapCmp->IsVelocityActive()) {
              trapCmp->ElectronVelocity(x, y, z, vx, vy, vz, trapMed, status);
            } else {
              trapMed->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
            }
            trapMed->ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
            trapCmp->ElectronAttachment(x, y, z, eta);
          } else {
            if (trapCmp->IsVelocityActive()) {
              trapCmp->HoleVelocity(x, y, z, vx, vy, vz, trapMed, status);
            } else {
              trapMed->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
            }
            trapMed->HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
            trapCmp->HoleAttachment(x, y, z, eta);
          }
        }
      } else {
        if (type < 0) {
          medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
          medium->ElectronTownsend(ex, ey, ez, bx, by, bz, alpha);
          medium->ElectronAttachment(ex, ey, ez, bx, by, bz, eta);
        } else {
          medium->HoleVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz);
          medium->HoleTownsend(ex, ey, ez, bx, by, bz, alpha);
          medium->HoleAttachment(ex, ey, ez, bx, by, bz, eta);
        }
      }
      vdx += wg[j] * vx;
      vdy += wg[j] * vy;
      vdz += wg[j] * vz;
      alphas[i] += wg[j] * alpha;
      etas[i] += wg[j] * eta;
    }
    // Compute the scaling factor for the projected length.
    double scale = 1.;
    if (m_useEquilibration) {
      const double vd = sqrt(vdx * vdx + vdy * vdy + vdz * vdz);
      if (vd * del <= 0.) {
        scale = 0.;
      } else {
        const double dinv = delx * vdx + dely * vdy + delz * vdz;
        if (dinv < 0.) {
          scale = 0.;
        } else {
          scale = (delx * vdx + dely * vdy + delz * vdz) / (vd * del);
        }
      }
    }
    alphas[i] *= 0.5 * del * scale;
    etas[i] *= 0.5 * del * scale;
  }

  // Skip equilibration if projection has not been requested.
  if (!m_useEquilibration) return true;
  if (!Equilibrate(alphas)) {
    if (m_debug) {
      std::cerr << m_className << "::ComputeAlphaEta:\n"
                << "    Unable to even out alpha steps.\n"
                << "    Calculation is probably inaccurate.\n";
    }
    return false;
  }
  if (!Equilibrate(etas)) {
    if (m_debug) {
      std::cerr << m_className << "::ComputeAlphaEta:\n"
                << "    Unable to even out alpha steps.\n"
                << "    Calculation is probably inaccurate.\n";
    }
    return false;
  }
  // Seems to have worked.
  return true;
}

bool AvalancheMC::Equilibrate(std::vector<double>& alphas) const {

  const unsigned int nPoints = alphas.size();
  // Try to alpha-equilibrate the returning parts.
  for (unsigned int i = 0; i < nPoints - 1; ++i) {
    if (alphas[i] >= 0.) continue;
    // Targets for subtracting
    double sub1 = -alphas[i] / 2.;
    double sub2 = sub1;
    bool try1 = false;
    bool try2 = false;
    // Try to subtract half in earlier points.
    for (unsigned int j = 0; j < i - 1; ++j) {
      if (alphas[i - j] > sub1) {
        alphas[i - j] -= sub1;
        alphas[i] += sub1;
        sub1 = 0.;
        try1 = true;
        break;
      } else if (alphas[i - j] > 0.) {
        alphas[i] += alphas[i - j];
        sub1 -= alphas[i - j];
        alphas[i - j] = 0.;
      }
    }
    // Try to subtract the other half in later points.
    for (unsigned int j = 0; j < nPoints - i - 1; ++j) {
      if (alphas[i + j] > sub2) {
        alphas[i + j] -= sub2;
        alphas[i] += sub2;
        sub2 = 0.;
        try2 = true;
        break;
      } else if (alphas[i + j] > 0.) {
        alphas[i] += alphas[i + j];
        sub2 -= alphas[i + j];
        alphas[i + j] = 0.;
      }
    }

    // Done if both sides have margin left.
    bool done = false;
    if (try1 && try2) {
      done = true;
    } else if (try1) {
      sub1 = -alphas[i];
      for (unsigned int j = 0; j < i - 1; ++j) {
        if (alphas[i - j] > sub1) {
          alphas[i - j] -= sub1;
          alphas[i] += sub1;
          sub1 = 0.;
          done = true;
          break;
        } else if (alphas[i - j] > 0.) {
          alphas[i] += alphas[i - j];
          sub1 -= alphas[i - j];
          alphas[i - j] = 0.;
        }
      }
    } else if (try2) {
      // Try upper side again.
      sub2 = -alphas[i];
      for (unsigned int j = 0; j < nPoints - i - 1; ++j) {
        if (alphas[i + j] > sub2) {
          alphas[i + j] -= sub2;
          alphas[i] += sub2;
          sub2 = 0.;
          done = true;
          break;
        } else if (alphas[i + j] > 0.) {
          alphas[i] += alphas[i + j];
          sub2 -= alphas[i + j];
          alphas[i + j] = 0.;
        }
      }
    }
    // See whether we succeeded.
    if (!done) return false;
  }
  return true;

}

void AvalancheMC::ComputeSignal(const double q) {

  const unsigned int nPoints = m_drift.size();
  if (nPoints < 2) return;
  for (unsigned int i = 0; i < nPoints - 1; ++i) {
    const double dt = m_drift[i + 1].t - m_drift[i].t;
    const double dx = m_drift[i + 1].x - m_drift[i].x;
    const double dy = m_drift[i + 1].y - m_drift[i].y;
    const double dz = m_drift[i + 1].z - m_drift[i].z;
    const double x =  m_drift[i].x + 0.5 * dx;
    const double y =  m_drift[i].y + 0.5 * dy;
    const double z =  m_drift[i].z + 0.5 * dz;
    m_sensor->AddSignal(q, m_drift[i].t, dt, x, y, z,
                        dx / dt, dy / dt, dz / dt);
  }
}

void AvalancheMC::ComputeInducedCharge(const double q) {

  if (m_drift.size() < 2) return;
  m_sensor->AddInducedCharge(q, m_drift[0].x, m_drift[0].y, m_drift[0].z,
                             m_drift.back().x, m_drift.back().y, m_drift.back().z);
}
}
