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

ViewField::ViewField() :
  className("ViewField"),
  debug(false), 
  useStatus(false),
  sensor(0), component(0),
  pxmin(-1.), pymin(-1.), pxmax(1.), pymax(1.),
  fmin(0.), fmax(100.),
  nContours(nMaxContours),
  nSamples1d(1000), nSamples2dX(200), nSamples2dY(200),
  canvas(0), hasExternalCanvas(false),
  fPot(0), fPotProfile(0) {

  SetDefaultProjection();
  plottingEngine.SetDefaultStyle();

}

ViewField::~ViewField() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;
  if (fPot != 0) delete fPot;
  if (fPotProfile != 0) delete fPotProfile;
  
}

void
ViewField::SetSensor(Sensor* s) {

  if (s == 0) {
    std::cerr << className << "::SetSensor:\n";
    std::cerr << "    Sensor pointer is null.\n";
    return;
  }

  sensor = s; 
  component = 0;
  // Get bounding box.
  bool ok = sensor->GetArea(pxmin, pymin, pzmin, pxmax, pymax, pzmax);
  // Get the voltage range.
  ok = sensor->GetVoltageRange(fmin, fmax);

}

void
ViewField::SetComponent(ComponentBase* c) {

  if (c == 0) {
    std::cerr << className << "::SetComponent:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  component = c;
  sensor = 0;
  // Get the bounding box.
  bool ok = component->GetBoundingBox(pxmin, pymin, pzmin, 
                                      pxmax, pymax, pzmax);
  // Get the voltage range.
  ok = component->GetVoltageRange(fmin, fmax);

}

void
ViewField::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
ViewField::SetArea(double xmin, double ymin, double zmin, 
                   double xmax, double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    std::cerr << className << "::SetArea:\n";
    std::cerr << "    Null area range not permitted.\n";
    std::cerr << "      " << xmin << " < x < " << xmax << "\n";
    std::cerr << "      " << ymin << " < y < " << ymax << "\n";
    std::cerr << "      " << zmin << " < z < " << zmax << "\n";
    return;
  } 
  pxmin = std::min(xmin,xmax);
  pymin = std::min(ymin,ymax);
  pzmin = std::min(zmin,zmax);
  pxmax = std::max(xmin,xmax);
  pymax = std::max(ymin,ymax);
  pzmax = std::max(zmin,zmax);
  
}

void 
ViewField::SetVoltageRange(const double minval, const double maxval) {

  fmin = std::min(minval, maxval);
  fmax = std::max(minval, maxval);
  
}

void
ViewField::SetNumberOfContours(const int n) {

  if (n <= nMaxContours) {
    nContours = n;
  } else {
    std::cerr << className << "::SetNumberOfContours:\n";
    std::cerr << "    Max. number of contours is " << nMaxContours << ".\n";
  }
  
}

void
ViewField::SetNumberOfSamples1d(const int n) {

  const int nmin = 10;
  const int nmax = 100000;

  if (n < nmin || n > nmax) {
    std::cerr << className << "::SetNumberOfSamples1d:\n";
    std::cerr << "    Number of points (" << n << ") out of range.\n";
    std::cerr << "    " << nmin << " <= n <= " << nmax << "\n";
    return;
  }

  nSamples1d = n;

}

void
ViewField::SetNumberOfSamples2d(const int nx, const int ny) {

  const int nmin = 10;
  const int nmax = 10000;
  if (nx < nmin || nx > nmax) {
    std::cerr << className << "::SetNumberOfSamples2d:\n";
    std::cerr << "    Number of x-points (" << nx <<  ") out of range.\n";
    std::cerr << "    " << nmin << " <= nx <= " << nmax << "\n";
  } else {
    nSamples2dX = nx;
  }

  if (ny < nmin || ny > nmax) {
    std::cerr << className << "::SetNumberOfSamples2d:\n";
    std::cerr << "    Number of y-points (" << ny <<  ") out of range.\n";
    std::cerr << "    " << nmin << " <= ny <= " << nmax << "\n";
  } else {
    nSamples2dY = ny;
  }

}

void 
ViewField::PlotContour() {

  // Setup the canvas
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle("Field View");
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();
  canvas->Range(pxmin, pymin, pxmax, pymax);

  if (fPot == 0) CreateFunction();

  fPot->SetRange(pxmin, pymin, pxmax, pymax);

  double level[nMaxContours];
  for (int i = 0; i < nContours; ++i) {
    if (nContours > 1) {
      level[i] = fmin + i * (fmax - fmin) / (nContours - 1.);
    } else {
      level[i] = (fmax + fmin) / 2.;
    }
  }
  fPot->SetContour(nContours, level);
  
  if (debug) {
    std::cout << className << "::PlotContour:\n";
    std::cout << "    Number of contours: " << nContours << "\n";
    for (int i = 0; i < nContours; ++i) {
      std::cout << "        Level " << i << " = " << level[i] << "\n";
    }
  }
  fPot->SetNpx(nSamples2dX);
  fPot->SetNpy(nSamples2dY);
  fPot->GetXaxis()->SetTitle(xLabel);
  fPot->GetYaxis()->SetTitle(yLabel);
  fPot->SetTitle("Contours of the potential");
  fPot->Draw("CONT4Z");
  canvas->Update();

}

void 
ViewField::PlotSurface() {

  // Setup the canvas
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle("Field View");
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();
  canvas->Range(pxmin, pymin, pxmax, pymax);
  
  if (fPot == 0) CreateFunction();

  fPot->SetRange(pxmin, pymin, pxmax, pymax);
  fPot->SetNpx(nSamples2dX); 
  fPot->SetNpy(nSamples2dY);
  fPot->GetXaxis()->SetTitle(xLabel);
  fPot->GetYaxis()->SetTitle(yLabel);
  fPot->SetTitle("Surface plot of the potential");
  fPot->Draw("SURF4");
  canvas->Update();

}

void 
ViewField::PlotProfile(const double x0, const double y0, const double z0,
                       const double x1, const double y1, const double z1) {

  // Check the distance between the two points.
  const double d = sqrt(pow(x1 - x0, 2) + pow(y1 - y0, 2) + pow(z1 - z0, 2));
  if (d <= 0.) {
    std::cerr << className << "::PlotProfile:\n";
    std::cerr << "    Start and end point coincide.\n";
    return;
  }
  
  // Setup the canvas.
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->SetTitle("Field View");
    if (hasExternalCanvas) hasExternalCanvas = false;
  }
  canvas->cd();
  
  if (fPotProfile == 0) CreateProfileFunction();

  fPotProfile->SetParameter(0, x0);
  fPotProfile->SetParameter(1, y0);
  fPotProfile->SetParameter(2, z0);
  fPotProfile->SetParameter(3, x1);
  fPotProfile->SetParameter(4, y1);
  fPotProfile->SetParameter(5, z1);
  if (debug) {
    std::cout << className << "::PlotProfile:\n";
    std::cout << "    Plotting potential along\n";
    std::cout << "    (" << fPotProfile->GetParameter(0) << ", "
                         << fPotProfile->GetParameter(1) << ", "
                         << fPotProfile->GetParameter(2) <<") - ("
                         << fPotProfile->GetParameter(3) << ", "
                         << fPotProfile->GetParameter(4) << ", "
                         << fPotProfile->GetParameter(5) << ")\n";
  }
  fPotProfile->GetXaxis()->SetTitle("normalised distance");
  fPotProfile->GetYaxis()->SetTitle("potential [V]");
  fPotProfile->SetTitle("Profile plot of the potential");
  fPotProfile->SetNpx(nSamples1d);
  fPotProfile->Draw();
  canvas->Update();

}

void
ViewField::CreateFunction() {

  int idx = 0;
  std::string fname = "fPotential_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "fPotential_";
    ss  << idx;
    fname = ss.str();
  }

  fPot = new TF2(fname.c_str(), this, &ViewField::EvaluatePotential, 
        pxmin, pxmax, pymin, pymax, 0, 
        "ViewField", "EvaluatePotential");
 
}

void
ViewField::CreateProfileFunction() {

  int idx = 0;
  std::string fname = "fPotentialProfile_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "fPotentialProfile_";
    ss  << idx;
    fname = ss.str();
  }

  const int nParameters = 6;
  fPotProfile = new TF1(fname.c_str(), 
        this, &ViewField::EvaluatePotentialProfile, 
        0., 1., nParameters, 
        "ViewField", "EvaluatePotentialProfile");
 
}

void 
ViewField::SetDefaultProjection() {

  // Default projection: x-y at z=0
  project[0][0] = 1;  project[1][0] = 0;  project[2][0] = 0;
  project[0][1] = 0;  project[1][1] = 1;  project[2][1] = 0;
  project[0][2] = 0;  project[1][2] = 0;  project[2][2] = 0;

  // Plane description
  plane[0] = 0;
  plane[1] = 0;
  plane[2] = 1;
  plane[3] = 0;

  // Prepare axis labels.
  Labels();

}

double
ViewField::EvaluatePotential(double* pos, double* par) {

  if (sensor == 0 && component == 0) return 0.;

  // Compute the field.
  double ex = 0., ey = 0., ez = 0., volt = 0.;
  int status = 0;
  Medium* medium;
  const double xpos = project[0][0] * pos[0] + project[1][0] * pos[1] + 
                      project[2][0];
  const double ypos = project[0][1] * pos[0] + project[1][1] * pos[1] + 
                      project[2][1];
  const double zpos = project[0][2] * pos[0] + project[1][2] * pos[1] + 
                      project[2][2];
 
  if (sensor == 0) {
    component->ElectricField(xpos, ypos, zpos, 
                             ex, ey, ez, volt, medium, status);
  } else {
    sensor->ElectricField(xpos, ypos, zpos, 
                          ex, ey, ez, volt, medium, status);
  }
  if (debug) {
    std::cout << className << "::EvaluatePotential:\n";
    std::cout << "    At (u, v) = (" 
              << pos[0] << ", " << pos[1] << "), "
              << " (x,y,z) = (" 
              << xpos << "," << ypos << "," << zpos << ")\n";
    std::cout << "    E = " << ex << ", " << ey << ", " << ez << "), V = "
              << volt << ", status = " << status << "\n";
  }
 
  if (useStatus && status != 0) volt = 0.;
 
  // Return the potential.
  return volt;
    
}

double
ViewField::EvaluatePotentialProfile(double* pos, double* par) {

  if (sensor == 0 && component == 0) return 0.;

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
  Medium* medium;
  
  if (sensor == 0) {
    component->ElectricField(x0 + t * dx, y0 + t * dy, z0 + t * dz,
                             ex, ey, ez, volt, medium, status);    
  } else {
    sensor->ElectricField(x0 + t * dx, y0 + t * dy, z0 + t * dz, 
                          ex, ey, ez, volt, medium, status);
  }

  if (useStatus && status != 0) volt = 0.;

  // Return the potential.
  return volt;
    
}

void ViewField::Labels() {

  // Initialisation of the x-axis label
  strcpy(xLabel,"\0");
  char buf[100];

  // x portion
  if (fabs(project[0][0] - 1) < 1.0e-4) {
    strcat(xLabel, "x");
  } else if (fabs(project[0][0] + 1) < 1.0e-4) {
    strcat(xLabel, "-x");
  } else if (project[0][0] > 1.0e-4) {
    sprintf(buf, "%g x", project[0][0]);
    strcat(xLabel, buf);
  } else if (project[0][0] < -1.0e-4) {
    sprintf(buf, "%g x", project[0][0]);
    strcat(xLabel, buf);
  }

  // y portion
  if(strlen(xLabel) > 0) {
    if (project[0][1] < -1.0e-4) {
      strcat(xLabel," - ");
    } else if (project[0][1] > 1.0e-4) {
      strcat(xLabel," + ");
    }
    if (fabs(project[0][1] - 1) < 1.0e-4 || 
	fabs(project[0][1] + 1) < 1.0e-4) {
      strcat(xLabel, "y");
    } else if (fabs(project[0][1]) > 1.0e-4) {
      sprintf(buf, "%g y", fabs(project[0][1]));
      strcat(xLabel, buf);
    }
  } else {
    if (fabs(project[0][1] - 1) < 1.0e-4) {
      strcat(xLabel, "y");
    } else if (fabs(project[0][1] + 1) < 1.0e-4) {
      strcat(xLabel, "-y");
    } else if (project[0][1] > 1.0e-4) {
      sprintf(buf, "%g y", project[0][1]);
      strcat(xLabel, buf);
    } else if (project[0][1] < -1.0e-4) {
      sprintf(buf, "%g y", project[0][1]);
      strcat(xLabel, buf);
    }
  }

  // z portion
  if(strlen(xLabel) > 0) {
    if (project[0][2] < -1.0e-4) {
      strcat(xLabel," - ");
    } else if (project[0][2] > 1.0e-4) {
      strcat(xLabel," + ");
    }
    if (fabs(project[0][2] - 1) < 1.0e-4 || 
	fabs(project[0][2] + 1) < 1.0e-4) {
      strcat(xLabel, "z");
    } else if (fabs(project[0][2]) > 1.0e-4) {
      sprintf(buf, "%g z", fabs(project[0][2]));
      strcat(xLabel, buf);
    }
  } else {
    if (fabs(project[0][2] - 1) < 1.0e-4) {
      strcat(xLabel, "z");
    } else if (fabs(project[0][2]+1) < 1.0e-4) {
      strcat(xLabel, "-z");
    } else if (project[0][2] > 1.0e-4) {
      sprintf(buf, "%g z", project[0][2]);
      strcat(xLabel, buf);
    } else if (project[0][2] < -1.0e-4) {
      sprintf(buf, "%g z", project[0][2]);
      strcat(xLabel, buf);
    }
  }

  // Unit
  strcat(xLabel, " [cm]");

  // Initialisation of the y-axis label
  strcpy(yLabel,"\0");

  // x portion
  if (fabs(project[1][0] - 1) < 1.0e-4) {
    strcat(yLabel, "x");
  } else if (fabs(project[1][0] + 1) < 1.0e-4) {
    strcat(yLabel, "-x");
  } else if (project[1][0] > 1.0e-4) {
    sprintf(buf, "%g x", project[1][0]);
    strcat(yLabel, buf);
  } else if (project[1][0] < -1.0e-4) {
    sprintf(buf, "%g x", project[1][0]);
    strcat(yLabel, buf);
  }

  // y portion
  if(strlen(yLabel) > 0) {
    if (project[1][1] < -1.0e-4) {
      strcat(yLabel," - ");
    } else if (project[1][1] > 1.0e-4) {
      strcat(yLabel," + ");
    }
    if (fabs(project[1][1] - 1) < 1.0e-4 || 
	fabs(project[1][1] + 1) < 1.0e-4) {
      strcat(yLabel, "y");
    } else if (fabs(project[1][1]) > 1.0e-4) {
      sprintf(buf, "%g y", fabs(project[1][1]));
      strcat(yLabel, buf);
    }
  } else {
    if (fabs(project[1][1] - 1) < 1.0e-4) {
      strcat(yLabel, "y");
    } else if (fabs(project[1][1] + 1) < 1.0e-4) {
      strcat(yLabel, "-y");
    } else if (project[1][1] > 1.0e-4) {
      sprintf(buf, "%g y", project[1][1]);
      strcat(yLabel, buf);
    } else if (project[1][1] < -1.0e-4) {
      sprintf(buf, "%g y", project[1][1]);
      strcat(yLabel, buf);
    }
  }

  // z portion
  if(strlen(yLabel) > 0) {
    if (project[1][2] < -1.0e-4) {
      strcat(yLabel," - ");
    } else if (project[1][2] > 1.0e-4) {
      strcat(yLabel," + ");
    }
    if (fabs(project[1][2] - 1) < 1.0e-4 || 
	fabs(project[1][2] + 1) < 1.0e-4) {
      strcat(yLabel, "z");
    } else if (fabs(project[1][2]) > 1.0e-4) {
      sprintf(buf, "%g z", fabs(project[1][2]));
      strcat(yLabel, buf);
    }
  } else {
    if (fabs(project[1][2] - 1) < 1.0e-4) {
      strcat(yLabel, "z");
    } else if (fabs(project[1][2] + 1) < 1.0e-4) {
      strcat(yLabel, "-z");
    } else if (project[1][2] > 1.0e-4) {
      sprintf(buf, "%g z", project[1][2]);
      strcat(yLabel, buf);
    } else if (project[1][2] < -1.0e-4) {
      sprintf(buf, "%g z", project[1][2]);
      strcat(yLabel, buf);
    }
  }

  // Unit
  strcat(yLabel, " [cm]");
 
  // Initialisation of the y-axis label
  strcpy(description,"\0");

  // x portion
  if (fabs(plane[0] - 1) < 1.0e-4) {
    strcat(description, "x");
  } else if (fabs(plane[0] + 1) < 1.0e-4) {
    strcat(description, "-x");
  } else if (plane[0] > 1.0e-4) {
    sprintf(buf, "%g x", plane[0]);
    strcat(description, buf);
  } else if (plane[0] < -1.0e-4) {
    sprintf(buf, "%g x", plane[0]);
    strcat(description, buf);
  }

  // y portion
  if(strlen(description) > 0) {
    if (plane[1] < -1.0e-4) {
      strcat(description," - ");
    } else if (plane[1] > 1.0e-4) {
      strcat(description," + ");
    }
    if (fabs(plane[1] - 1) < 1.0e-4 || 
        fabs(plane[1] + 1) < 1.0e-4) {
      strcat(description, "y");
    } else if (fabs(plane[1]) > 1.0e-4) {
      sprintf(buf, "%g y", fabs(plane[1]));
      strcat(description, buf);
    }
  } else {
    if (fabs(plane[1] - 1) < 1.0e-4) {
      strcat(description, "y");
    } else if (fabs(plane[1] + 1) < 1.0e-4) {
      strcat(description, "-y");
    } else if (plane[1] > 1.0e-4) {
      sprintf(buf, "%g y", plane[1]);
      strcat(description, buf);
    } else if (plane[1] < -1.0e-4) {
      sprintf(buf, "%g y", plane[1]);
      strcat(description, buf);
    }
  }

  // z portion
  if(strlen(description) > 0) {
    if (plane[2] < -1.0e-4) {
      strcat(description," - ");
    } else if (plane[2] > 1.0e-4) {
      strcat(description," + ");
    }
    if (fabs(plane[2] - 1) < 1.0e-4 || 
	fabs(plane[2] + 1) < 1.0e-4) {
      strcat(description, "z");
    } else if (fabs(plane[2]) > 1.0e-4) {
      sprintf(buf, "%g z", fabs(plane[2]));
      strcat(description, buf);
    }
  } else {
    if (fabs(plane[2] - 1) < 1.0e-4) {
      strcat(description, "z");
    } else if (fabs(plane[2] + 1) < 1.0e-4) {
      strcat(description, "-z");
    } else if (plane[2] > 1.0e-4) {
      sprintf(buf, "%g z", plane[2]);
      strcat(description, buf);
    } else if (plane[2] < -1.0e-4) {
      sprintf(buf, "%g z", plane[2]);
      strcat(description, buf);
    }
  }

  // Constant
  sprintf(buf, " = %g", plane[3]);
  strcat(description, buf);

  if (debug) {
    std::cout << className << "::Labels:\n";
    std::cout << "    x label: |" << xLabel << "|\n";
    std::cout << "    y label: |" << yLabel << "|\n";
    std::cout << "    plane:   |" << description << "|\n";
  }

}

void 
ViewField::SetPlane(const double fx, const double fy, const double fz,
                    const double x0, const double y0, const double z0) {

  // Calculate 2 in-plane vectors for the normal vector
  double fnorm = sqrt(fx * fx + fy * fy + fz * fz);
  if (fnorm > 0 && fx * fx + fz * fz > 0) {
    project[0][0] =  fz                /  sqrt(fx * fx + fz * fz);
    project[0][1] =  0;
    project[0][2] = -fx                /  sqrt(fx * fx + fz * fz);
    project[1][0] = -fx * fy           / (sqrt(fx * fx + fz * fz) * fnorm);
    project[1][1] = (fx * fx + fz * fz)/ (sqrt(fx * fx + fz * fz) * fnorm);
    project[1][2] = -fy * fz           / (sqrt(fx * fx + fz * fz) * fnorm);
    project[2][0] =  x0;
    project[2][1] =  y0;
    project[2][2] =  z0;
  } else if (fnorm > 0 && fy * fy + fz * fz > 0) {
    project[0][0] =  (fy * fy + fz * fz) / (sqrt(fy * fy + fz * fz) * fnorm);
    project[0][1] = -fx * fz             / (sqrt(fy * fy + fz * fz) * fnorm);
    project[0][2] = -fy * fz             / (sqrt(fy * fy + fz * fz) * fnorm);
    project[1][0] =  0;
    project[1][1] =  fz                  / sqrt(fy * fy + fz * fz);
    project[1][2] = -fy                  / sqrt(fy * fy + fz * fz);
    project[2][0] =  x0;
    project[2][1] =  y0;
    project[2][2] =  z0;
  } else {
    std::cout << className << "::SetPlane:\n";
    std::cout << "    Normal vector has zero norm.\n";
    std::cout << "    No new projection set.\n";
  }

  // Store the plane description
  plane[0] = fx;
  plane[1] = fy;
  plane[2] = fz;
  plane[3] = fx * x0 + fy * y0 + fz * z0;

  // Make labels to be placed along the axes
  Labels();

}

void 
ViewField::Rotate(const double angle) {

  // Rotate the axes
  double auxu[3], auxv[3];
  for (int i = 0; i < 3; ++i) {
    auxu[i] = cos(angle) * project[0][i] - sin(angle) * project[1][i];
    auxv[i] = sin(angle) * project[0][i] + cos(angle) * project[1][i];
  }
  for (int i = 0; i < 3; ++i) {
    project[0][i] = auxu[i];
    project[1][i] = auxv[i];
  }

  // Make labels to be placed along the axes
  Labels();
  
}

}
