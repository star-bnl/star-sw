#include <iostream>
#include <math.h>

#include "DriftLineRKF.hh"
#include "FundamentalConstants.hh"
#include "GarfieldConstants.hh"

namespace Garfield {

DriftLineRKF::DriftLineRKF() :
  sensor(0), medium(0), 
  maxStepSize(1.e8), intAccuracy(1.e-8), 
  maxSteps(1000), 
  usePlotting(false), viewer(0), 
  debug(false) {
  
  path.clear();

}


void 
DriftLineRKF::SetSensor(Sensor* s) {
    
  if (s == 0) {
    std::cerr << "DriftLineRKF::SetSensor:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }

  sensor = s;

}

void
DriftLineRKF::EnablePlotting(ViewDrift* view) {

  if (view == 0) {
    std::cerr << "DriftLineRKF::EnablePlotting:\n";
    std::cerr << "    Viewer is not defined.\n";
    return;
  }
  
  usePlotting = true;
  viewer = view;

}

void
DriftLineRKF::DisablePlotting() {

  viewer = 0;
  usePlotting = false;

}

void 
DriftLineRKF::DriftLine(double x0, double y0, double z0, double t0,
                        std::string particleType) {

  if (usePlotting) {
    viewer->NewElectronDriftLine(1, iLine, x0, y0, z0);
  }

  // Check if the sensor is defined
  if (sensor == 0) {
    std::cerr << "DriftLineRKF::DriftLine:\n";
    std::cerr << "    Sensor is not defined.\n";
    return;
  }
  
  // Check to make sure initial position is in a 
  // valid location ie. non zero field, 
  // in a drift medium.
  
  // Get field values
  double ex, ey, ez, eTot; 
  double bx, by, bz, bTot;
  int status;

  sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  sensor->ElectricField(x0, y0, z0, ex, ey, ez, medium, status);
  if (status != 0) {
    std::cerr << "DriftLineRKF::DriftLine:\n";
    std::cerr << "    No valid field at initial position.\n";
    return;
  }

  eTot = sqrt(ex * ex + ey * ey + ez * ez);
  bTot = sqrt(bx * bx + by * by + bz * bz);

  // Approximation parameters
  const double c10 = 214. / 891.; 
  const double c11 =   1. / 33.;
  const double c12 = 650. / 891.;
  const double c20 = 533. / 2106.; 
  const double c22 = 800. / 1053.; 
  const double c23 =  -1. / 78.;

  const double b10 = 1. / 4.; 
  const double b20 = -189. / 800.; 
  const double b21 = 729. / 800.;
  const double b30 = 214. / 891.; 
  const double b31 = 1. / 33.; 
  const double b32 = 650./891.;

  // Current position
  double r[3] = {x0, y0, z0};
  // Estimate for next step
  double r1[3] = {0., 0., 0.};
  // Initial velocity
  double v0[3] = {0., 0., 0.};
  // Velocities at mid-points
  double v1[3] = {0., 0., 0.};
  double v2[3] = {0., 0., 0.};
  double v3[3] = {0., 0., 0.};
  // Position where particle has crossed the trap radius of a wire
  double rc[3] = {0., 0., 0.}; 

  //final velocity estimates
  double phi1[3], phi2[3];

  // Initialize particle velocity
  // Add if clause for electron/hole/ion.
  if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v0[0], v0[1], v0[2])) {
    std::cerr << "DriftLineRKF::DriftLine:\n";
    std::cerr << "    Failed to retrieve drift velocity.\n";
    return;
  }
  double vTot = sqrt(v0[0] * v0[0] + v0[1] * v0[1] + v0[2] * v0[2]);

  // Time step and previous time step
  std::cout <<"Electron Velocity (cm/ns): ("<< v0[0] <<", " << v0[1] <<", " << v0[2] <<") = " << vTot << "\n";
  double dt = intAccuracy / vTot;
  double pdt = 0.;
  
  // Count the number of steps
  int counter = 0;

  // Continue with the next step of drift if true
  bool keepGoing = true;

  path.clear();
  while (counter <= maxSteps && keepGoing) {
    int iWire; //keep a reference to the wire which traps the drift line
    step tempStep;
    path.push_back(tempStep);
    path[counter].xi = r[0];
    path[counter].yi = r[1];
    path[counter].zi = r[2];
    if (counter == 0) path[counter].ti = t0;
    else path[counter].ti = path[counter -1].tf;

    // First estimate of new drift velocity
    r1[0] = r[0] + dt * b10 * v0[0];
    r1[1] = r[1] + dt * b10 * v0[1];
    r1[2] = r[2] + dt * b10 * v0[2];
    sensor->MagneticField(r1[0], r1[1], r1[2], bx, by, bz, status);
    sensor->ElectricField(r1[0], r1[1], r1[2], ex, ey, ez, medium, status);
    if (status == 0) {
      if (sensor->IsWireCrossed(path.back().xi, path.back().yi, path.back().zi, 
                                r1[0], r1[1], r1[2], rc[0], rc[1], rc[2])) {
         std::cerr<<"DriftLineRKF::DriftLine:\n\tdrift line crossed wire. Abandoning.";
         path[counter].status = "Crossed Wire.";
         break;
      }
      else if(sensor->IsInTrapRadius(r1[0], r1[1], r1[2], xWire, yWire, rWire)){
         DriftToWire(r1[0], r1[1], r1[2], iWire);
         break;
      }
      else{
         if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v1[0], v1[1], v1[2])) {
           std::cerr << "DriftLineRKF::DriftLine:\n";
           std::cerr << "    Failed to retrieve drift velocity.\n";
           return;
         }      
      }
    } 
    else {
      EndDriftLine();
      break;
    }
    // Second estimate of new drift velocity
    r1[0] = r[0] + dt * (b20 * v0[0] + b21 * v1[0]);
    r1[1] = r[1] + dt * (b20 * v0[1] + b21 * v1[1]);
    r1[2] = r[2] + dt * (b20 * v0[2] + b21 * v1[2]);
    sensor->MagneticField(r1[0], r1[1], r1[2], bx, by, bz, status);
    sensor->ElectricField(r1[0], r1[1], r1[2], ex, ey, ez, medium, status);
    if (status == 0) {
      if (sensor->IsWireCrossed(path.back().xi, path.back().yi, path.back().zi, 
                                r1[0], r1[1], r1[2], rc[0], rc[1], rc[2])) {
         std::cerr<<"DriftLineRKF::DriftLine:\n\tdrift line crossed wire. Abandoning.";
         path[counter].status = "Crossed Wire.";
         break;
      }
      else if(sensor->IsInTrapRadius(r1[0], r1[1], r1[2], xWire, yWire, rWire)){
         DriftToWire(r1[0], r1[1], r1[2], iWire);
         break;
      }
      else{
        if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v2[0], v2[1], v2[2])) {
          std::cerr << "DriftLineRKF::DriftLine:\n";
          std::cerr << "    Failed to retrieve drift velocity.\n";
          return;
        }
      }     
    } 
    else {
      EndDriftLine();
      break;
    }
   // Third estimate of new drift velocity
    r1[0] = r[0] + dt * (b30 * v0[0] + b31 * v1[0] + b32 * v2[0]);
    r1[1] = r[1] + dt * (b30 * v0[1] + b31 * v1[1] + b32 * v2[1]);
    r1[2] = r[2] + dt * (b30 * v0[2] + b31 * v1[2] + b32 * v2[2]);   
    sensor->MagneticField(r1[0],r1[1],r1[2],bx, by, bz, status);
    sensor->ElectricField(r1[0],r1[1],r1[2],ex, ey, ez, medium, status);
    if (status == 0) {
      if (sensor->IsWireCrossed(path.back().xi, path.back().yi, path.back().zi, 
                                r1[0], r1[1], r1[2], rc[0], rc[1], rc[2])) {
         std::cerr<<"DriftLineRKF::DriftLine:\n\tdrift line crossed wire. Abandoning.";
         path[counter].status = "Crossed Wire.";
         break;
      }
      else if(sensor->IsInTrapRadius(r1[0], r1[1], r1[2], xWire, yWire, rWire)){
         DriftToWire(r1[0], r1[1], r1[2], iWire);
         break;
      }
      else{
        if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, v3[0], v3[1], v3[2])) {
          std::cerr << "DriftLineRKF::DriftLine:\n";
          std::cerr << "    Failed to retrieve drift velocity.\n";
          return;
        }       
      }
    } 
    else {
      EndDriftLine();
      break;
    }
    // Calculate estimates of velocity over step
    phi1[0] = c10 * v0[0] + c11 * v1[0] + c12 * v2[0];
    phi1[1] = c10 * v0[1] + c11 * v1[1] + c12 * v2[1];
    phi1[2] = c10 * v0[2] + c11 * v1[2] + c12 * v2[2];
    
    phi2[0] = c20 * v0[0] + c22 * v2[0] + c23 * v3[0];
    phi2[1] = c20 * v0[1] + c22 * v2[1] + c23 * v3[1];
    phi2[2] = c20 * v0[2] + c22 * v2[2] + c23 * v3[2];
    // Check step length is valid
    double stepLength = sqrt(phi1[0] * phi1[0] + phi1[1] * phi1[1] + phi1[2] * phi1[2]);
    if (stepLength <= 0.0){
      std::cerr << "DriftLineRKF::DriftLine::\n\t" 
                << "Step length zero. Abandoning drift.\n";
      keepGoing = false;
    } else if (dt * stepLength > maxStepSize) {
      if (debug) {
        std::cerr << "DriftLineRKF::DriftLine::\n\t" 
                  << "Step length too long. Reducing time step.\n";
      }
      dt = 0.5 * maxStepSize / stepLength;
    } else {
      if (debug) {
        std::cout << "DriftLineRKF::DriftLine::\n\t" 
                  << "Step good.\n";
      }
    }
    pdt = dt;
    // Update position
    r[0] += dt * phi1[0];
    r[1] += dt * phi1[1];
    r[2] += dt * phi1[2];

    path[counter].xf = r[0];
    path[counter].yf = r[1];
    path[counter].zf = r[2];
    path[counter].tf = path[counter].ti + dt;
 
    sensor->ElectricField(r[0], r[1], r[2], ex, ey, ez, medium, status);
    if (status != 0) {
      if (debug) std::cout << "Outside bounds!\n";
      EndDriftLine();
      break;
    }
    /*if(sensor->IsTrapped(x0,y0)){
      if (debug) std::cout<<"\tElectron trapped by wire.\n";
      keepGoing = false;
      }*/

    // Adjust step size depending on accuracy
    if (phi1[0] != phi2[0] || phi1[1] != phi2[1] || phi1[2] != phi2[2]) {
      if (debug) {
        std::cout << "DriftLineRKF::DriftLine:\n\t" 
                   << "Adapting step size.\n";
      }
      dt = sqrt(dt * intAccuracy / 
                (fabs(phi1[0] - phi2[0]) + fabs(phi1[1] - phi2[1]) + fabs(phi1[2] - phi2[2])));
    } else {
      if (debug) {
        std::cout << "DriftLineRKF::DriftLine:\n\t" 
                  << "Increasing step size.\n";
      }
      dt *= 2.;
    }
    // Make sure that dt is different from zero; 
    // this should always be ok.
    if (dt <= 0.) {
      std::cerr << "DriftLineRKF::DriftLine:\n";
      std::cerr << "    Step size is zero (program bug).\n";
      std::cerr << "    The calculation is abandoned.\n";
      return;
    }
    // Skipped something about initial step sizes and previous step size

    // Prevent step size growing to fast
    if (dt > 10. * pdt) {
      dt = 10. * pdt;
    }

    // Stop in case dt tends to become too small.
    if (dt * (fabs(phi1[0]) + fabs(phi1[1]) + fabs(phi1[2])) < intAccuracy) {
      if (debug) {
        std::cerr << "DriftLineRKF::DriftLine:\n";
        std::cerr << "    Step size has become smaller than int. accuracy.\n";
        std::cerr << "    The calculation is abandoned.\n";
        return;
      }
    }

    // Update velocity
    v0[0] = v3[0];
    v0[1] = v3[1];
    v0[2] = v3[2];
    
    if (keepGoing && counter <= maxSteps) {
      path[counter].status = "alive";
    } else if (counter > maxSteps) {
       path[counter].status = "maxStep";
    } else {
       path[counter].status = "Abandoned";
    }
    // Increase counter (default counter max = 1000)
    counter++;
  }
  // If the user specifies output step history
  std::cout << "Step #\t\ttime\t\tXi\t\tYi\t\tZi\t\tdt\t\tStatus\n";
  for(int i = 0; i < path.size() ; i++){
    std::cout.precision(8);
    std::cout<<i<<"\t\t"<<path[i].ti<<"\t\t"
	     <<path[i].xi<<"\t\t"
	     <<path[i].yi<<"\t\t"
	     <<path[i].zi<<"\t\t"
	     <<fabs(path[i].tf - path[i].ti)<<"\t\t"
	     <<path[i].status <<"\n";
  }
  std::cout<<path.size()-1<<"\t\t"<<path.back().tf 
           <<"\t\t" <<path.back().xf 
           <<"\t\t"<<path.back().yf
           <<"\t\t"<<path.back().zf
           <<"\t\t---\t\tEND\n"; 
  for (int i = 0; i < path.size(); i++){
    if (usePlotting) {
      viewer->AddDriftLinePoint(iLine, path[i].xi, path[i].yi, path[i].zi);
    }
  }
  if (usePlotting) {
      viewer->AddDriftLinePoint(iLine,path.back().xf,
				path.back().yf,
				path.back().zf);
  }

}

bool
DriftLineRKF::CheckStep(double x, double y, double z, bool status){
}

void
DriftLineRKF::DriftToWire(double x0, double y0, double z0, int iWire) {

  if(debug || true){ 
    std::cout<<"Particle trapped by wire at: ";
    std::cout<<x0 <<", " <<y0 <<", " <<z0<<" = "<<sqrt(x0*x0 + y0*y0 + z0*z0) <<"\n";
    std::cout<<"By wire located at (" << xWire <<", " 
             << yWire <<") with physical radius " 
             << rWire <<" cm.\n";
   }
 
  bool lastStep = false;
  double timeToDrift = 0.;

  // Check to make sure initial position has non-zero field
  double ex, ey, ez;
  double bx, by, bz;
  int status;
  sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  sensor->ElectricField(x0, y0, z0, ex, ey, ez, medium, status);
  if(status != 0){
    std::cerr<<"DriftLineRKF::DriftToWire:\n\t";
    std::cerr<<"Zero field at initial position. Abandoning drift to wire.\n";
    path.back().status = "Zero field. Abandoned.";
    return;
 }
  
 // Estimate time to wire
 double vx0 = 0.;
 double vz0 = 0.;
 double vy0 = 0.;
 if(!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx0, vy0, vz0)){
   std::cerr<<"DriftLineRKF::DriftToWire:\n\t";
   std::cerr<<"Unable to retrieve drift velocity.\n";
   return;
 }

 double speed0 = sqrt( vx0 * vx0 + vy0 * vy0 + vz0 * vz0 );
 double dist2wire = DistanceToWire(x0, y0, z0);
 double tCrude = dist2wire / speed0;

 std::cout<<speed0<<"\n";

 // Check if tCrude is to small
 if(tCrude < 1.e-6){
   path.back().xf = x0;
   path.back().yf = y0;
   path.back().zf = z0;
   path.back().tf = path.back().ti + tCrude;
   path.back().status = "Distance to wire to small.";
   return;
 }

 while(!lastStep){
   
   // Estimate where the drift-line with end up
   double x1 = x0 + tCrude * vx0;
   double y1 = y0 + tCrude * vy0;
   double z1 = z0 + tCrude * vz0;
   if(debug){
     std::cout<<"Step to wire: " << x1 << ", " << y1 <<", " << z1 << "\n";
   }
   
   // Check to make sure step is in a good location
   dist2wire = DistanceToWire(x1, y1, z1);
   if(dist2wire < 0.){
     
     if(debug){
       std::cout<<"DriftLineRKF::DriftToWire:\n\t";
       std::cout<<"Dirft line inside wire. This may be the last step.\n";
     }
     
     lastStep = true;
   
     // Move the end point outside the wire.
     while(dist2wire < 0.){
       tCrude *= 0.9999;
       x1 = x0 + tCrude * vx0;
       y1 = y0 + tCrude * vy0;
       z1 = z0 + tCrude * vz0;
       dist2wire = DistanceToWire(x1,y1,z1);
     }
   }
   
   sensor->MagneticField(x1, y1, z1, bx, by, bz, status);
   sensor->ElectricField(x1, y1, z1, ex, ey, ez, medium, status);
   if(status != 0){
     std::cerr<<"DriftLineRKF::DriftToWire:\n\t";
     std::cerr<<"Zero field at step location ("<<x1 <<", " <<y1 <<", " << z0 <<"). Abandoning.\n";
     std::cerr<<"Status returned: " << status << ".\n";
     path.back().status = "Zero field. Abandoned.";
     return;
   }

   // Now Calculate the drift velocity at this end point
   double vx1 = 0.;
   double vy1 = 0.;
   double vz1 = 0.;
   if(!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx1, vy1, vz1)){
     std::cerr << "DriftLineRKF::DriftToWire:\n\t";
     std::cerr<< "Unable to retreive drift veloctiy. Abandoning.\n";
     path.back().status = "Abandoned";
   }
   
   double speed1 = sqrt( vx1 * vx1 + vy1 * vy1 + vz1 * vz1 );
   
   // Calculate a mid point between (x0, y0) and (x1,y1)

   double xm = 0.5 * (x0 + x1);
   double ym = 0.5 * (y0 + y1);
   double zm = 0.5 * (z0 + z1);

   // Check mid point location and find velocity

   sensor->MagneticField(xm, ym, zm, bx, by, bz, status);
   sensor->ElectricField(xm, ym, zm, ex, ey, ez, medium, status);
   if(status != 0){
     std::cerr<<"DriftLineRKF::DriftToWire:\n\t";
     std::cerr<<"Zero field at step location ("<<xm <<", " <<ym <<", " << zm <<"). Abandoning.\n";
     path.back().status = "Zero field. Abandoned.";
     return;
   }
   
   // Now Calculate the drift velocity at this mid point
   
   double vxm = 0.;
   double vym = 0.;
   double vzm = 0.;
   if(!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vxm, vym, vzm)){
     std::cerr<<"DriftLineRKF::DriftToWire:\n\t";
     std::cerr<<"Unable to retreive drift veloctiy. Abandoning.\n";
     path.back().status = "Abandoned";
   }
   double speedm = sqrt( vxm * vxm + vym * vym + vzm * vzm);
   
   // Compare the first and second order estimates
   
   double stepLength = sqrt( pow(x0 - x1, 2) + pow(y0 - y1, 2) );
   
   if(stepLength * fabs( 1./speed0 - 2./speedm + 1./speed1) / 3.< 1.e-4*(1+path.back().ti)){
     // Accuracy is good enough
     timeToDrift += stepLength * ( 1./speed0 + 4./speedm + 1./speed1)/6.;
     path.back().xf = x1;
     path.back().yf = y1;
     path.back().zf = z1;
     // Proceed to the next step
     x0 = x1;
     y0 = y1;
     z0 = z1;
     vx0 = vx1;
     vy0 = vy1;
     vz0 = vz1;
   }
   else{
     // Accuracy was not good enough so half the step time
     tCrude *= 0.5;
     lastStep = false;
   }
 }
 path.back().status = "Drifted to wire.";
 path.back().tf = path.back().ti + timeToDrift;
}

double
DriftLineRKF::DistanceToWire(double x, double y, double z){

  return sqrt( pow(xWire - x, 2) + pow(yWire - y, 2) ) - rWire;

}

void 
DriftLineRKF::EndDriftLine() {


  double x,y,z;
  double vx,vy,vz;
  double bx,by,bz;
  double ex,ey,ez;
  double lastStepLength;
  int status;

  //these will store the original position for use later in time calculation
  double xp = path.back().xi;
  double yp = path.back().yi;
  double zp = path.back().zi;
  double x0 = xp, y0 = yp, z0 = zp;
  double x1 = xp, y1 = yp, z1 = zp;
  
  sensor->MagneticField(x0, y0, z0, bx, by, bz, status);
  sensor->ElectricField(x0, y0, z0, ex, ey, ez, medium, status);
  if (status != 0) {
    std::cerr << "DriftLineRKF::EndDriftLine:\n";
    std::cerr << "    No valid field at initial point.\n";
    std::cerr << "    Program bug!\n";
    return;
  }
  if (!medium->ElectronVelocity(ex, ey, ez, bx, by, bz, vx, vy, vz)) {
    std::cerr << "DriftLineRKF::EndDriftLine:\n";
    std::cerr << "    Failed to retrieve drift velocity.\n";
    return;
  }

  double speed = sqrt(vx * vx + vy * vy + vz * vz);

  // x1, y1, z1 to store beginning of previous step for now.
  if (path.size() > 1) {
    x1 = path[path.size() - 2].xi;
    y1 = path[path.size() - 2].yi;
    z1 = path[path.size() - 2].zi;
  }
  // TODO: Do something for single point case.
 
  lastStepLength = sqrt(pow(fabs(x1-x0),2) +
			pow(fabs(y1-y0),2) +
			pow(fabs(z1-z0),2));
 
  x1 = x0;
  y1 = y0;
  z1 = z0;

  // Add steps sizes equal to the last step size until you leave the volume
  // steps added in same direction as previous step
  bool keepGoing = true;
  while (keepGoing) {
    sensor->ElectricField(x1, y1, z1, ex, ey, ez, medium, status);
    // TODO: Check also if inside the drift area.
    if (status != 0) {
      keepGoing = false;
      break;
    }
    x1 += lastStepLength * vx / speed;
    y1 += lastStepLength * vy / speed;
    z1 += lastStepLength * vz / speed;
  }

  for (int i = 0; i < 100; i++) {
    x = x0 + 0.5 * (x1 - x0);
    y = y0 + 0.5 * (y1 - y0);
    z = z0 + 0.5 * (z1 - z0);
    sensor->ElectricField(x, y, z, ex, ey, ez, medium, status);
    if (status == 0) {
      x0 = x;
      y0 = y;
      z0 = z;
    }  else {
      x1 = x;
      y1 = y;
      z1 = z;
    }  
  }
  
  // Add final step to path
  path.back().xf = x;
  path.back().yf = y;
  path.back().zf = z;
  path.back().tf = path.back().ti + fabs(sqrt(pow((x - xp), 2) + pow((y - yp), 2) + pow((z - zp),2))) / speed; 
  path.back().status = "left volume";

}

}

