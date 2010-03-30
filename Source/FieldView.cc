#include <stdio.h>
#include <string.h>
#include <math.h>

#include <TAxis.h> 
#include <TCanvas.h>
#include <TF2.h> 

#include "FieldView.hh"

namespace Garfield {

namespace sharesensor {

  Sensor* sensor;
  double matrix[3][3];
  
  double FieldViewFunction(double* pos, double* par) {
  
    // Debugging
    bool debug = false;
    
    // Compute the field
    double ex, ey, ez, volt;
    int status;
    Medium* medium;
    const double xpos = sharesensor::matrix[0][0] * pos[0] + 
                        sharesensor::matrix[1][0] * pos[1] + 
                        sharesensor::matrix[2][0];
    const double ypos = sharesensor::matrix[0][1] * pos[0] + 
                        sharesensor::matrix[1][1] * pos[1] + 
                        sharesensor::matrix[2][1];
    const double zpos = sharesensor::matrix[0][2] * pos[0] + 
                        sharesensor::matrix[1][2] * pos[1] + 
                        sharesensor::matrix[2][2];
    
    sensor->ElectricField(xpos, ypos, zpos, ex, ey, ez, volt, medium, status);
    if (debug) {
      printf("At (u,v) = (%g,%g), (x,y,z) = (%g,%g,%g), E = (%g,%g,%g), V = %g, status = %d\n",
             pos[0], pos[1], xpos, ypos, zpos, ex, ey, ez, volt, status);
    }
    
    // Return the potential
    return volt;
    
  }

}

FieldView::FieldView(Sensor* s):
  debug(false),
  nContours(20),
  fcont(0) {
  
  // Debugging
  if (debug) printf("FieldView: Constructor called.\n");

  // Access to the sensor
  sharesensor::sensor = s;

  // Set default projection
  Default();

  // Get default range
  bool ok = s->GetArea(pxmin, pymin, pzmin, pxmax, pymax, pzmax);

  // Get voltage range
  ok = s->GetVoltageRange(fmin, fmax);

}

void 
FieldView::Area(double xmin, double ymin, double zmin, 
                double xmax, double ymax, double zmax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax || zmin == zmax) {
    printf("FieldView::Area: Null area range not permitted.\n");
    printf("                 %g < x < %g\n", xmin, xmax);
    printf("                 %g < y < %g\n", ymin, ymax);
    printf("                 %g < z < %g\n", zmin, zmax);
  } else {
    pxmin = std::min(xmin,xmax);
    pymin = std::min(ymin,ymax);
    pzmin = std::min(zmin,zmax);
    pxmax = std::max(xmin,xmax);
    pymax = std::max(ymin,ymax);
    pzmax = std::max(zmin,zmax);
  }
  
}

void 
FieldView::Contour(char* function, TCanvas* c) {

  // Plot the contour
  //  gROOT->SetStyle("igrfstyle");

  // Store the projection information
  for (int i=0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      sharesensor::matrix[i][j] = project[i][j];
    }
  }

  // Prepare the contours
  if (fcont != 0) {
    delete fcont;
    fcont = 0;
  }
  fcont = new TF2("fcont", sharesensor::FieldViewFunction, pxmin, pxmax, pymin, pymax, 0);
  const int nLevelMax = 20;
  double level[nLevelMax];
  if (nContours > nLevelMax) nContours = nLevelMax;
  for (int i = 0; i < nContours; ++i) {
    if (nContours > 1) {
      level[i] = fmin + i * (fmax - fmin) / (nContours - 1.);
    } else {
      level[i] = (fmax + fmin) / 2.;
    }
  }
  fcont->SetContour(nContours, level);
  
  if (debug) {
    printf("FieldView: Number of contours: %d\n", nContours);
    for (int i = 0; i < nContours; ++i) {
      printf("                Level %d = %g\n", i, level[i]);
    }
  }
  c->Range(pxmin, pymin, pxmax, pymax);
  fcont->SetNpx(50);
  fcont->SetNpy(50);
  // fcont->SetBit(kCannotPick);
  fcont->GetXaxis()->SetTitle(xLabel);
  fcont->GetYaxis()->SetTitle(yLabel);
  fcont->SetTitle("Contours of V");
  fcont->Draw("cont1");

}

void 
FieldView::Range(const double minval, const double maxval) {

  fmin = std::min(minval,maxval);
  fmax = std::max(minval,maxval);
  
}

void 
FieldView::Default() {

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
FieldView::Plane(const double fx, const double fy, const double fz,
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
    printf("FieldView::Plane: Normal vector has zero norm. No new projection set.\n");
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
