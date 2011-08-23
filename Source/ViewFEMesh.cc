// Some code was copied/modified from ViewField.cc
#include <iostream>
#include <cmath>

#include "ComponentFieldMap.hh"
#include "Plotting.hh"
#include "ViewFEMesh.hh"

namespace Garfield {

ViewFEMesh::ViewFEMesh() :
  className("ViewCell"), 
  label("Cell Layout"),
  debug(false),
  fillMesh(false),
  canvas(0), hasExternalCanvas(false),
  hasUserArea(false),
  xMin(-1.), yMin(-1.), zMin(-1.), 
  xMax( 1.), yMax( 1.), zMax( 1.),
  component(0),
  viewDrift(0) {

  plottingEngine.SetDefaultStyle();
  SetDefaultProjection();
  
}

ViewFEMesh::~ViewFEMesh() {

  if (!hasExternalCanvas && canvas != 0) delete canvas;

}

void
ViewFEMesh::SetComponent(ComponentFieldMap* comp) {

  if (comp == 0) {
    std::cerr << className << "::SetComponent:\n";
    std::cerr << "    Component pointer is null.\n";
    return;
  }

  component = comp;

} 
  
void
ViewFEMesh::SetCanvas(TCanvas* c) {

  if (c == 0) return;
  if (!hasExternalCanvas && canvas != 0) {
    delete canvas;
    canvas = 0;
  }
  canvas = c;
  hasExternalCanvas = true;

}

void 
ViewFEMesh::SetArea(double xmin, double ymin,
                   double xmax, double ymax) {

  // Check range, assign if non-null
  if (xmin == xmax || ymin == ymax) {
    std::cout << className << "::SetArea:\n";
    std::cout << "    Null area range not permitted.\n";
    return;
  }
  xMin = std::min(xmin, xmax);
  yMin = std::min(ymin, ymax);
  xMax = std::max(xmin, xmax);
  yMax = std::max(ymin, ymax);

  zMin = 0.;
  zMax = 1.;

  hasUserArea = true;
 
}

void
ViewFEMesh::SetArea() {

  hasUserArea = false;

}

// The plotting functionality here is ported from Garfield
//  with some inclusion of code from ViewCell.cc
bool
ViewFEMesh::Plot() {

  if (component == 0) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Component is not defined.\n";
    return false;
  }
  
  double pmin = 0., pmax = 0.;
  if (!component->GetVoltageRange(pmin, pmax)) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Component is not ready.\n";
    return false;
  }

  // Get the bounding box.
  if (!hasUserArea) {
    std::cerr << className << "::Plot:\n";
    std::cerr << "    Bounding box cannot be determined.\n";
    std::cerr << "    Call SetArea first.\n";
    return false;
  }

  // Set up a canvas if one does not already exist.
  if (canvas == 0) {
    canvas = new TCanvas();
    canvas->Range(xMin, yMin, xMax, yMax);
    canvas->SetTitle(label.c_str());
    if (hasExternalCanvas) hasExternalCanvas = false;
  }

  // Plot the elements
  DrawElements();  
  canvas->Update();

  return true;

}

// Set the projection plane: modified from ViewField.cc 
//  to match functionality of Garfield
void 
ViewFEMesh::SetPlane(const double fx, const double fy, const double fz,
                    const double x0, const double y0, const double z0) {

  // Calculate 2 in-plane vectors for the normal vector
  double fnorm = sqrt(fx * fx + fy * fy + fz * fz);
  double dist = fx * x0 + fy * y0 + fz * z0;
  if (fnorm > 0 && fx * fx + fz * fz > 0) {
    project[0][0] =  fz                /  sqrt(fx * fx + fz * fz);
    project[0][1] =  0;
    project[0][2] = -fx                /  sqrt(fx * fx + fz * fz);
    project[1][0] = -fx * fy           / (sqrt(fx * fx + fz * fz) * fnorm);
    project[1][1] = (fx * fx + fz * fz)/ (sqrt(fx * fx + fz * fz) * fnorm);
    project[1][2] = -fy * fz           / (sqrt(fx * fx + fz * fz) * fnorm);
    project[2][0] =  dist*fx/(fnorm*fnorm);
    project[2][1] =  dist*fy/(fnorm*fnorm);
    project[2][2] =  dist*fz/(fnorm*fnorm);
  } else if (fnorm > 0 && fy * fy + fz * fz > 0) {
    project[0][0] =  (fy * fy + fz * fz) / (sqrt(fy * fy + fz * fz) * fnorm);
    project[0][1] = -fx * fz             / (sqrt(fy * fy + fz * fz) * fnorm);
    project[0][2] = -fy * fz             / (sqrt(fy * fy + fz * fz) * fnorm);
    project[1][0] =  0;
    project[1][1] =  fz                  / sqrt(fy * fy + fz * fz);
    project[1][2] = -fy                  / sqrt(fy * fy + fz * fz);
    project[2][0] =  dist*fx/(fnorm*fnorm);
    project[2][1] =  dist*fy/(fnorm*fnorm);
    project[2][2] =  dist*fz/(fnorm*fnorm);
  } else {
    std::cout << className << "::SetPlane:\n";
    std::cout << "    Normal vector has zero norm.\n";
    std::cout << "    No new projection set.\n";
  }

  // Store the plane description
  plane[0] = fx;
  plane[1] = fy;
  plane[2] = fz;
  plane[3] = dist;

}

// Set the default projection for the plane: copied from ViewField.cc
void 
ViewFEMesh::SetDefaultProjection() {

  // Default projection: x-y at z=0
  project[0][0] = 1;  project[1][0] = 0;  project[2][0] = 0;
  project[0][1] = 0;  project[1][1] = 1;  project[2][1] = 0;
  project[0][2] = 0;  project[1][2] = 0;  project[2][2] = 0;

  // Plane description
  plane[0] = 0;
  plane[1] = 0;
  plane[2] = 1;
  plane[3] = 0;

}

// Use ROOT plotting functions to draw the mesh elements on the canvas
// General methodology ported from Garfield
void
ViewFEMesh::DrawElements() {

  // Get the map boundaries from the component
  double mapxmax = component->mapxmax;
  double mapxmin = component->mapxmin;
  double mapymax = component->mapymax;
  double mapymin = component->mapymin;
  double mapzmax = component->mapzmax;
  double mapzmin = component->mapzmin;

  // Get the periodicities.
  double sx = mapxmax-mapxmin;
  double sy = mapymax-mapymin;
  double sz = mapzmax-mapzmin;
  const bool perX = component->xPeriodic || component->xMirrorPeriodic;
  const bool perY = component->yPeriodic || component->yMirrorPeriodic;
  const bool perZ = component->zPeriodic || component->zMirrorPeriodic;

  // Clear the meshes and drift line lists
  mesh.clear();
  driftLines.clear();

  // Prepare the final projection matrix (the transpose of the 2D array "project")
  double fnorm = sqrt(plane[0] * plane[0] + plane[1] * plane[1] + plane[2] * plane[2]);
  TArrayD dataProj(9);
  dataProj[0] = project[0][0]; dataProj[1] = project[1][0]; dataProj[2] = plane[0]/fnorm;
  dataProj[3] = project[0][1]; dataProj[4] = project[1][1]; dataProj[5] = plane[1]/fnorm;
  dataProj[6] = project[0][2]; dataProj[7] = project[1][2]; dataProj[8] = plane[2]/fnorm;
  TMatrixD projMat(3,3,dataProj.GetArray());

  // Calculate the determinant of the projection matrix
  double projDet = projMat(0,0)*(projMat(1,1)*projMat(2,2) - projMat(1,2)*projMat(2,1)) -
                 projMat(0,1)*(projMat(1,0)*projMat(2,2) - projMat(1,2)*projMat(2,0)) +
                 projMat(0,2)*(projMat(1,0)*projMat(2,1) - projMat(1,1)*projMat(2,0));

  // Calculate the inverse of the projection matrix for
  //  calculating coordinates in the viewing plane            
  if(projDet != 0) { projMat.Invert(); }
  else {
    std::cerr << className << "::DrawElements:\n";
    std::cerr << "    Projection matrix is not invertible.\n";
    std::cerr << "    Finite element mesh will not be drawn.\n";
  }

  // Get the plane information
  double fx = plane[0];
  double fy = plane[1];
  double fz = plane[2];
  double dist = plane[3];

  // Construct two empty single-column matrices for use as coordinate vectors
  TMatrixD xMat(3,1);

  // Determine the number of periods present in the cell.
  int nMaxX = 0, nMinX = 0;
  int nMaxY = 0, nMinY = 0;
  int nMaxZ = 0, nMinZ = 0;
  if (perX) {
    nMinX = int(xMin / sx) - 1;
    nMaxX = int(xMax / sx) + 1;
  }
  if (perY) {
    nMinY = int(yMin / sy) - 1;
    nMaxY = int(yMax / sy) + 1;
  }
  if (perZ) {
    nMinZ = int(zMin / sz) - 1;
    nMaxZ = int(zMax / sz) + 1;
  }

  // Loop over all elements
  for(int elem = 0; elem < component->nElements; elem++)  {

    // Do not plot the drift medium
    if(component->materials[component->elements[elem].matmap].driftmedium) 
      continue;
    
    // -- Tetrahedral elements
    
    // Coordinates of vertices
    double vx1, vy1, vz1;
    double vx2, vy2, vz2;
    double vx3, vy3, vz3;
    double vx4, vy4, vz4;

    // Get the color for this element (default to 1)
    int colorID = colorMap.count(component->elements[elem].matmap);
    if(colorID != 0) colorID = colorMap[component->elements[elem].matmap];
    else colorID = 1;

    // Loop over the periodicities in x
    for(int nx = nMinX; nx <= nMaxX; nx++) {

      // Determine the x-coordinates of the tetrahedral vertices
      if(component->xMirrorPeriodic && nx != 2*(nx/2)) {
        vx1 = mapxmin + (mapxmax - component->nodes[component->elements[elem].emap[0]].x) + sx*nx;
        vx2 = mapxmin + (mapxmax - component->nodes[component->elements[elem].emap[1]].x) + sx*nx;
        vx3 = mapxmin + (mapxmax - component->nodes[component->elements[elem].emap[2]].x) + sx*nx;
        vx4 = mapxmin + (mapxmax - component->nodes[component->elements[elem].emap[3]].x) + sx*nx;
      }
      else {
        vx1 = component->nodes[component->elements[elem].emap[0]].x + sx*nx;
        vx2 = component->nodes[component->elements[elem].emap[1]].x + sx*nx;
        vx3 = component->nodes[component->elements[elem].emap[2]].x + sx*nx;
        vx4 = component->nodes[component->elements[elem].emap[3]].x + sx*nx;
      }

      // Loop over the periodicities in y
      for(int ny = nMinY; ny <= nMaxY; ny++) {

        // Determine the y-coordinates of the tetrahedral vertices
        if(component->yMirrorPeriodic && ny != 2*(ny/2)) {
          vy1 = mapymin + (mapymax - component->nodes[component->elements[elem].emap[0]].y) + sy*ny;
          vy2 = mapymin + (mapymax - component->nodes[component->elements[elem].emap[1]].y) + sy*ny;
          vy3 = mapymin + (mapymax - component->nodes[component->elements[elem].emap[2]].y) + sy*ny;
          vy4 = mapymin + (mapymax - component->nodes[component->elements[elem].emap[3]].y) + sy*ny;
        }
        else {
          vy1 = component->nodes[component->elements[elem].emap[0]].y + sy*ny;
          vy2 = component->nodes[component->elements[elem].emap[1]].y + sy*ny;
          vy3 = component->nodes[component->elements[elem].emap[2]].y + sy*ny;
          vy4 = component->nodes[component->elements[elem].emap[3]].y + sy*ny;
        }

        // Loop over the periodicities in z
        for(int nz = nMinZ; nz <= nMaxZ; nz++) {

          // Determine the z-coordinates of the tetrahedral vertices
          if(component->zMirrorPeriodic && nz != 2*(nz/2)) {
            vz1 = mapzmin + (mapzmax - component->nodes[component->elements[elem].emap[0]].z) + sz*nz;
            vz2 = mapzmin + (mapzmax - component->nodes[component->elements[elem].emap[1]].z) + sz*nz;
            vz3 = mapzmin + (mapzmax - component->nodes[component->elements[elem].emap[2]].z) + sz*nz;
            vz4 = mapzmin + (mapzmax - component->nodes[component->elements[elem].emap[3]].z) + sz*nz;
          }
          else {
            vz1 = component->nodes[component->elements[elem].emap[0]].z + sz*nz;
            vz2 = component->nodes[component->elements[elem].emap[1]].z + sz*nz;
            vz3 = component->nodes[component->elements[elem].emap[2]].z + sz*nz;
            vz4 = component->nodes[component->elements[elem].emap[3]].z + sz*nz;
          }

          // Store the x and y coordinates of the relevant mesh vertices
          std::vector<double> vX;
          std::vector<double> vY;

          // Boolean values for determining what coordinates are in the viewing plane
          bool in1 = false, in2 = false, in3 = false, in4 = false;

          // Boolean value for determining whether a plane cut succeeded
          bool planeCut = false;

          // Value used to determine whether a vertex is in the plane
          double pcf = 
           std::max(std::max(std::max(std::max(std::max(std::max(std::abs(vx1),
            std::abs(vy1)), std::abs(vz1)), std::abs(fx)), std::abs(fy)),
            std::abs(fz)), std::abs(dist));

          // First isolate the vertices that are in the viewing plane
          if(std::abs(fx * vx1 + fy * vy1 + fz * vz1 - dist) < 1.0e-4 * pcf) 
            { in1 = true; }
          if(std::abs(fx * vx2 + fy * vy2 + fz * vz2 - dist) < 1.0e-4 * pcf) 
            { in2 = true; }
          if(std::abs(fx * vx3 + fy * vy3 + fz * vz3 - dist) < 1.0e-4 * pcf) 
            { in3 = true; }
          if(std::abs(fx * vx4 + fy * vy4 + fz * vz4 - dist) < 1.0e-4 * pcf) 
            { in4 = true; }

          // Calculate the planar coordinates for those edges that are in the plane
          if(in1) {
            PlaneCoords(vx1, vy1, vz1, projMat, xMat);
            vX.push_back(xMat(0, 0)); vY.push_back(xMat(1, 0));
          }
          if(in2) {
            PlaneCoords(vx2, vy2, vz2, projMat, xMat);
            vX.push_back(xMat(0, 0)); vY.push_back(xMat(1, 0));
          }
          if(in3) {
            PlaneCoords(vx3, vy3, vz3, projMat, xMat);
            vX.push_back(xMat(0, 0)); vY.push_back(xMat(1, 0));
          }
          if(in4) {
            PlaneCoords(vx4, vy4, vz4, projMat, xMat);
            vX.push_back(xMat(0, 0)); vY.push_back(xMat(1, 0));
          }

          // Cut the sides that are not in the plane
          if(!(in1 || in2)) {
            planeCut = PlaneCut(vx1, vy1, vz1, vx2, vy2, vz2, xMat);
            if(planeCut) { vX.push_back(xMat(0,0)); vY.push_back(xMat(1,0)); }
          }
          if(!(in1 || in3)) {
            planeCut = PlaneCut(vx1, vy1, vz1, vx3, vy3, vz3, xMat);
            if(planeCut) { vX.push_back(xMat(0,0)); vY.push_back(xMat(1,0)); }
          }
          if(!(in1 || in4)) {
            planeCut = PlaneCut(vx1, vy1, vz1, vx4, vy4, vz4, xMat);
            if(planeCut) { vX.push_back(xMat(0,0)); vY.push_back(xMat(1,0)); }
          }
          if(!(in2 || in3)) {
            planeCut = PlaneCut(vx2, vy2, vz2, vx3, vy3, vz3, xMat);
            if(planeCut) { vX.push_back(xMat(0,0)); vY.push_back(xMat(1,0)); }
          }
          if(!(in2 || in4)) {
            planeCut = PlaneCut(vx2, vy2, vz2, vx4, vy4, vz4, xMat);
            if(planeCut) { vX.push_back(xMat(0,0)); vY.push_back(xMat(1,0)); }
          }
          if(!(in3 || in4)) {
            planeCut = PlaneCut(vx3, vy3, vz3, vx4, vy4, vz4, xMat);
            if(planeCut) { vX.push_back(xMat(0,0)); vY.push_back(xMat(1,0)); }
          }

          // Create a TPolyLine object connecting the points
          if(vX.size() >= 3) {

            // Wrap around to the starting point
            vX.push_back(vX[0]);
            vY.push_back(vY[0]);

            // Create the TPolyLine
            int polyPts = 0;
            TPolyLine* poly = new TPolyLine();
            poly->SetLineColor(colorID);
            poly->SetFillColor(colorID);
            poly->SetLineWidth(1);
            for(int pt = 0; pt < (int) vX.size(); pt++)   {

              // Add this point if it is within the view to within 10%
              if(vX[pt] >= xMin - std::abs(xMin) * 0.1 
                  && vX[pt] <= xMax + std::abs(xMax) * 0.1 
                  && vY[pt] >= yMin - std::abs(yMin) * 0.1
                  && vY[pt] <= yMax + std::abs(yMax) * 0.1) {
                poly->SetPoint(polyPts, vX[pt], vY[pt]);
                polyPts++;
              }
            }
            mesh.push_back(poly);
          }
        } // end z-periodicity loop
      } // end y-periodicity loop
    } // end x-periodicity loop
  } // end loop over elements

  // If we have an associated ViewDrift, plot projections of the drift lines
  if(viewDrift != 0) {

    for(int dline = 0; dline < (int)viewDrift->driftLines.size(); dline++) {

      // Get the number of points and their coordinates
      float npts = viewDrift->driftLines[dline].GetN();
      float * pts = viewDrift->driftLines[dline].GetP();

      // Create a TPolyLine that is a 2D projection of the original
      TPolyLine* poly = new TPolyLine();
      poly->SetLineColor(viewDrift->driftLines[dline].GetLineColor());
      int polyPts = 0;
      for(int pt = 0; pt < npts; pt++) {

        // Get the coordinates of this point in the TPolyLine3D
        double ptx = pts[3*pt];
        double pty = pts[3*pt+1];
        double ptz = pts[3*pt+2];

        // Project this point onto the plane
        PlaneCoords(ptx, pty, ptz, projMat, xMat);

        // Add this point if it is within the view to within 10%
        if(xMat(0,0) >= xMin - std::abs(xMin) * 0.1 
            && xMat(0,0) <= xMax + std::abs(xMax) * 0.1 
            && xMat(1,0) >= yMin - std::abs(yMin) * 0.1
            && xMat(1,0) <= yMax + std::abs(yMax) * 0.1) {
          poly->SetPoint(polyPts, xMat(0,0), xMat(1,0));
          polyPts++;
        }
      } // end loop over points

      // Add the drift line to the list
      driftLines.push_back(poly);

    } // end loop over drift lines
  } // end if(viewDrift != 0)

  // Call the ROOT draw methods to plot the elements
  canvas->cd();
  
  // Draw the mesh on the canvas
  for(int m = mesh.size(); m--;) {
    if(fillMesh) mesh[m]->Draw("f:same");
    else mesh[m]->Draw("same");
  }

  // Draw the drift lines on the view
  for(int m = driftLines.size(); m--;) {
    driftLines[m]->Draw("same");
  }

}


// Ported from Garfield: determines the point of intersection, in planar 
//  coordinates, of a plane with the line connecting multiple points
// x1,y1,z1;x2,y2,z2: the world coordinates of the two points
// projMat;planeMat: the projection and plane matrices
// xMat: the resulting planar coordinates of the intersection point
bool
ViewFEMesh::PlaneCut(double x1, double y1, double z1, double x2, double y2, 
                     double z2, TMatrixD & xMat) {

  // Set up the matrix for cutting edges not in the plane
  TArrayD dataCut(9);
  TMatrixD cutMat(3,3);
  dataCut[0] = project[0][0];
  dataCut[1] = project[1][0];
  dataCut[2] = x1-x2;
  dataCut[3] = project[0][1];
  dataCut[4] = project[1][1];
  dataCut[5] = y1-y2;
  dataCut[6] = project[0][2];
  dataCut[7] = project[1][2];
  dataCut[8] = z1-z2;
  cutMat.SetMatrixArray(dataCut.GetArray());

  // Calculate the determinant of the cut matrix
  double cutDet = 
    cutMat(0,0)*(cutMat(1,1)*cutMat(2,2) - cutMat(1,2)*cutMat(2,1)) -
    cutMat(0,1)*(cutMat(1,0)*cutMat(2,2) - cutMat(1,2)*cutMat(2,0)) +
    cutMat(0,2)*(cutMat(1,0)*cutMat(2,1) - cutMat(1,1)*cutMat(2,0));

  // Do not proceed if the matrix is singular
  if(cutDet == 0) return false;

  // Set up a coordinate vector (RHS of equation)
  TArrayD dataCoords(3);
  TMatrixD coordMat(3,1);
  dataCoords[0] = x1 - project[2][0]; 
  dataCoords[1] = y1 - project[2][1]; 
  dataCoords[2] = z1 - project[2][2];
  coordMat.SetMatrixArray(dataCoords.GetArray());

  // Invert the cut matrix and multiply to get the solution
  cutMat.Invert(); 
  xMat = cutMat*coordMat;

  // Return success if the plane point is between the two vertices
  if(xMat(2,0) < 0 || xMat(2,0) > 1) return false;
  return true;
}

// Ported from Garfield: calculates the planar coordinates
// x,y,z: original world coordinates
// projMat: the projection matrix
// xMat: the resulting planar coordinates in single-column (vector) form
bool
ViewFEMesh::PlaneCoords(double x, double y, double z, 
                        const TMatrixD & projMat, TMatrixD & xMat) { 

  // Set up the coordinate vector
  TArrayD dataCoords(3);
  TMatrixD coordMat(3,1);    
  dataCoords[0] = x; dataCoords[1] = y; dataCoords[2] = z;
  coordMat.SetMatrixArray(dataCoords.GetArray());
  xMat = projMat*coordMat;

  return true;
}

}
