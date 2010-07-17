#include <iostream>
#include <sstream>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include <TROOT.h>
#include <TAxis.h> 
#include <TCanvas.h>
#include <TF2.h> 

#include "FieldView.hh"

namespace Garfield {

FieldView::FieldView() :
  debug(false), sensor(0),
  pxmin(-1.), pymin(-1.), pxmax(1.), pymax(1.),
  fmin(0.), fmax(100.),
  nContours(nMaxContours),
  canvas(0), hasExternalCanvas(false),
  fPot(0) {

  SetDefaultProjection();

}

FieldView::FieldView(Sensor* s) :
  debug(false), sensor(s),
  pxmin(-1.), pymin(-1.), pxmax(1.), pymax(1.),
  fmin(0.), fmax(100.),
  nContours(nMaxContours),
  canvas(0), hasExternalCanvas(false),
  fPot(0) {

  SetDefaultProjection();
  if (sensor != 0) {
    // Get default range
    bool ok = sensor->GetArea(pxmin, pymin, pzmin, pxmax, pymax, pzmax);
    // Get voltage range
    ok = sensor->GetVoltageRange(fmin, fmax);
  }

}

FieldView::~FieldView() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;
  if (fPot != 0) delete fPot;
}

void
FieldView::SetSensor(Sensor* s) {

  if (s == 0) {
    printf("FieldView::SetSensor:\n");
    printf("    Sensor is not defined.\n");
    return;
  }

  sensor = s; 
  // Get bounding box
  bool ok = sensor->GetArea(pxmin, pymin, pzmin, pxmax, pymax, pzmax);
  // Get voltage range
  ok = sensor->GetVoltageRange(fmin, fmax);

}

void
FieldView::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
FieldView::SetArea(double xmin, double ymin, double zmin, 
                   double xmax, double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    printf("FieldView::SetArea:\n");
    printf("    Null area range not permitted.\n");
    printf("                 %g < x < %g\n", xmin, xmax);
    printf("                 %g < y < %g\n", ymin, ymax);
    printf("                 %g < z < %g\n", zmin, zmax);
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
FieldView::SetVoltageRange(const double minval, const double maxval) {

  fmin = std::min(minval, maxval);
  fmax = std::max(minval, maxval);
  
}

void
FieldView::SetNumberOfContours(const int n) {

  if (n <= nMaxContours) {
    nContours = n;
  } else {
    printf("FieldView::SetNumberOfContours:\n");
    printf("    Max. number of contours is %d.\n", nMaxContours);
  }
  
}

void 
FieldView::PlotContour() {

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
    printf("FieldView::PlotContour:\n");
    printf("    Number of contours: %d\n", nContours);
    for (int i = 0; i < nContours; ++i) {
      printf("                Level %d = %g\n", i, level[i]);
    }
  }
  fPot->SetNpx(100);
  fPot->SetNpy(100);
  fPot->GetXaxis()->SetTitle(xLabel);
  fPot->GetYaxis()->SetTitle(yLabel);
  fPot->SetTitle("Contours of the potential");
  fPot->Draw("CONT4Z");
  canvas->Update();

}

void 
FieldView::PlotSurface() {

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
  fPot->GetXaxis()->SetTitle(xLabel);
  fPot->GetYaxis()->SetTitle(yLabel);
  fPot->SetTitle("Surface plot of the potential");
  fPot->Draw("SURF7");
  canvas->Update();

}

void
FieldView::CreateFunction() {

  int idx = 0;
  std::string fname = "fPotential_0";
  while (gROOT->GetListOfFunctions()->FindObject(fname.c_str())) {
    ++idx;
    std::stringstream ss;
    ss << "fPotential_";
    ss  << idx;
    fname = ss.str();
  }

  fPot = new TF2(fname.c_str(), this, &FieldView::EvaluatePotential, 
        pxmin, pxmax, pymin, pymax, 0, "FieldView", "EvaluatePotential");
 
}

void 
FieldView::SetDefaultProjection() {

  // Default projection: x-y at z=0
  project[0][0] = 1;  project[1][0] = 0;  project[2][0] = 0;
  project[0][1] = 0;  project[1][1] = 1;  project[2][1] = 0;
  project[0][2] = 0;  project[1][2] = 0;  project[2][2] = 0;

  // Plane description
  plane[0] = 0;
  plane[1] = 0;
  plane[2] = 1;
  plane[3] = 0;

  // Prepare axis lavels
  Labels();

}

double
FieldView::EvaluatePotential(double* pos, double* par) {

  if (sensor == 0) return 0.;

  // Compute the field
  double ex = 0., ey = 0., ez = 0., volt = 0.;
  int status = 0;
  Medium* medium;
  const double xpos = project[0][0] * pos[0] + project[1][0] * pos[1] + 
                      project[2][0];
  const double ypos = project[0][1] * pos[0] + project[1][1] * pos[1] + 
                      project[2][1];
  const double zpos = project[0][2] * pos[0] + project[1][2] * pos[1] + 
                      project[2][2];
 
  sensor->ElectricField(xpos, ypos, zpos, ex, ey, ez, volt, medium, status);
  if (debug) {
    printf("FieldView::EvaluatePotential:\n");
    printf("    At (u, v) = (%g, %g), (x,y,z) = (%g, %g, %g)\n", 
           pos[0], pos[1], xpos, ypos, zpos);
    printf("    E = (%g, %g, %g), V = %g, status = %d\n",
           ex, ey, ez, volt, status);
  }
  
  // Return the potential
  return volt;
    
}

void FieldView::Labels() {

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
    printf("FieldView::Labels: x label: |%s|\n", xLabel);
    printf("                   y label: |%s|\n", yLabel);
    printf("                   plane:   |%s|\n", description);
  }

}

void 
FieldView::SetPlane(const double fx, const double fy, const double fz,
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
    printf("FieldView::SetPlane:\n");
    printf("    Normal vector has zero norm. No new projection set.\n");
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
FieldView::Rotate(const double angle) {

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
