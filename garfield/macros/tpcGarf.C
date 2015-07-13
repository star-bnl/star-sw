#include <iostream>
#include <TROOT.h>
#include <TApplication.h>

#include "ViewField.hh"
#include "ViewCell.hh"
#include "ComponentAnalyticField.hh"
#include "MediumMagboltz.hh"
#include "SolidBox.hh"
#include "GeometrySimple.hh"
#include "Sensor.hh"
#include "ViewDrift.hh"
#include "FundamentalConstants.hh"
#include "DriftLineRKF.hh"
#include "ViewMedium.hh"
#include "ViewSignal.hh"
#include "Random.hh"
#include "TrackHeed.hh"

using namespace Garfield;
#if 0
int main(int argc, char * argv[]) {
  TApplication app("app", &argc, argv);
#else
  void tpcGarf() {
#endif
  
  // Switch between IROC and OROC
  const bool iroc = false;

  //gating ON/OFF
  bool gating = false;

  // Voltage settings (kV)
  double vSens = 1570.;   //OROC
  if(iroc) vSens = 1460.;
  const double vGate = -70.;
  const double deltav = 90.; //for closed gate mode
 
  // y-axis gap between rows of wires
  double gap = 0.3;     // O-ROCs
  if (iroc) gap = 0.2;  // I-ROCs
 
  // y coordinates of the wires
  const double ys = gap; //anode wires
  const double yc = 2. * gap;    //cathode
  const double yg = 2. * gap + 0.3;    //gate
 
  // Plane coordinates (drift field)
  const double yHV = 249.7; //in cm
  const double vHV = -100000;
 
  // Periodicity (wire spacing)
  const double period = 0.25;
  const int nRep = 10;
  
 
  const double dc = period ;
  const double dg = period / 2;
 
  // Wire diameters
  const double dSens = 0.0020;
  const double dCath = 0.0075;
  const double dGate = 0.0075;
 
  // Setup the gas
  const double pressure = 750.; //Torr
  const double temperature = 293.15; //K
 
  // Make a gas medium
  MediumMagboltz* gas = new MediumMagboltz();
  // Set the temperature [K] and pressure [Torr]
  gas->SetTemperature(temperature);
  gas->SetPressure(pressure);
  gas->SetComposition("ne", 85.72, "co2", 9.52, "n2", 4.76);

  //Read from .gas file
  gas->LoadGasFile("Ne_90_CO2_10_N2_5_with_mg.gas");
  gas->LoadIonMobility("../Data/IonMobility_Ne+_Ne.txt");

  // Build the geometry
  GeometrySimple* geo = new GeometrySimple();
  SolidBox* box = new SolidBox(0., yHV/2, 0,
                               (nRep) * period, yHV/2, 1);

  // Add the solid to the geometry, together with the medium inside
  geo->AddSolid(box, gas);

  // Setup the electric field
  ComponentAnalyticField* comp_e = new ComponentAnalyticField();
  comp_e->SetPeriodicityX(nRep * period);
  for (int i = 0; i < nRep; ++i) {
   comp_e->AddWire((i - 2.) * period, ys, dSens, vSens, "s");
  }
  for (int i = 0; i < nRep+5; ++i) {
    comp_e->AddWire(dc * (i + 0.5), yc, dCath, 0., "c");
  }
  if (gating){
    for (int i = 0; i < nRep * 2; i=i+2)
      comp_e->AddWire(dg * (i + 0.5), yg, dGate, vGate+deltav, "g+");
    for (int i = 1; i < nRep * 2; i=i+2)
      comp_e->AddWire(dg * (i + 0.5), yg, dGate, vGate-deltav, "g-");
  }
  else{
    for (int i = 0; i < nRep * 7; ++i) {
      comp_e->AddWire(dg * (i + 0.5), yg, dGate, vGate, "g", 100., 50., 19.3, 1);  //set trap radius to 1 to avoid stopping the drift line prematurely at the gating

    }
  }

  comp_e->AddPlaneY(0., 0., "pad_plane");
  comp_e->AddPlaneY(yHV, vHV, "HV");
 
 
  //Add magnetic field
  comp_e->SetMagneticField(0,0.5,0);
  comp_e->SetGeometry(geo);

   //create a canvas that will also be used for drift lines plotting
   TCanvas* myCanvas = new TCanvas();
   ViewCell* cellView = new ViewCell();
   cellView->SetComponent(comp_e);
   cellView->SetArea(-0.5,0,-2,0.5,5*gap,2);
   cellView->SetCanvas(myCanvas);
   cellView->Plot2d();
   myCanvas->Update();

  const int nWires = comp_e->GetNumberOfWires();
  for (int i = 0; i < nWires; ++i) {
    double xw, yw, dw, vw;
    std::string lbl;
    double lw, qw;
    int nw;
    comp_e->GetWire(i, xw, yw, dw, vw, lbl, lw, qw, nw);
    std::cout << i << "   " << lbl << "   " << xw << "   " << yw << "   " << nw << std::endl;
  }

  // Make a sensor for electrons (gating open)
  Sensor* sensor_e = new Sensor();
  sensor_e->AddComponent(comp_e);

  // Plot isopotential contours
  ViewField* fView = new ViewField;
  fView->SetSensor(sensor_e);
  fView->SetArea(-nRep * period / 2., 0.,
                  nRep * period / 2., 2);
  fView->SetVoltageRange(-400., 1000.);
  fView->PlotContour();
 
  ViewDrift* vd = new ViewDrift();
  vd->SetCanvas(myCanvas);
  vd->SetArea(-0.5,0,-2,0.5,5*gap,2);


  DriftLineRKF* driftline_e = new DriftLineRKF();
  driftline_e->SetSensor(sensor_e);
  driftline_e->EnablePlotting(vd);

  //ION part
 
  //Gating switches ON, so use another sensor
  gating = true;
  // Setup the electric field
  ComponentAnalyticField* comp_i = new ComponentAnalyticField();
  comp_i->SetPeriodicityX(nRep * period);
  for (int i = 0; i < nRep+10; ++i) {
   comp_i->AddWire((i - 2.) * period, ys, dSens, vSens, "s");
  }
  for (int i = 0; i < nRep +5; ++i) {
    comp_i->AddWire(dc * (i + 0.5), yc, dCath, 0., "c");
  }
  if (gating){
    for (int i = 0; i < nRep * 2; i=i+2)
      comp_i->AddWire(dg * (i + 0.5), yg, dGate, vGate+deltav, "g+");
    for (int i = 1; i < nRep * 2; i=i+2)
      comp_i->AddWire(dg * (i + 0.5), yg, dGate, vGate-deltav, "g-");
  }
  else{
    for (int i = 0; i < nRep * 7; ++i) {
      comp_i->AddWire(dg * (i + 0.5), yg, dGate, vGate, "g", 100., 50., 19.3, 1);  //set trap radius to 1 to avoid stopping the drift line prematurely at the gating

    }
  }
  comp_i->AddPlaneY(0., 0., "pad_plane");
  comp_i->AddPlaneY(yHV, vHV, "HV");
 
  //Add magnetic field
  comp_i->SetMagneticField(0,0.5,0);
  comp_i->SetGeometry(geo);

  // Make a sensor for ions (gating closed)
  Sensor* sensor_i = new Sensor();
  sensor_i->AddComponent(comp_i);
  sensor_i->AddElectrode(comp_i, "pad_plane");

  const double theta = 0.4;    //Polya parameter for gain distribution (for Ne)
  const double avg_gain = 5;

  double gain = RndmPolya(theta)*avg_gain; // Gain per avalanche follows a Polya distribution
  double r = 0.01;
  double dummy = 0.;
  std::string status="";
  double xendpoint = 0, yendpoint = gap;
  int plane = 0, cathode= 0, gate = 0, escape = 0;
  int k = 0;
  plane = 0; cathode = 0; gate = 0; escape = 0;
  DriftLineRKF* driftline_i = new DriftLineRKF();
  driftline_i->SetSensor(sensor_i);
  driftline_i->EnablePlotting(vd);
 
  // Track class
  double momentum = 1.e9;
  TrackHeed* track = new TrackHeed();
  track->SetSensor(sensor_e);
  track->SetParticle("pi");
  track->SetMomentum(momentum);
  double x0 = (-nRep+1)*period, y0 = 1, z0 = 0., t0 = 0.;
  double dx0 = 1., dy0 = 0., dz0 = 0.;
  double x,y,z,t,e,dx,dy,dz,xcl,ycl,zcl,tcl,ecl,extra;
  int ncl = 0, j = 0;

  track->NewTrack(x0, y0, z0, t0, dx0, dy0, dz0);
 
  const double dT = 0.0198;
  
  while (track->GetCluster(xcl, ycl, zcl, tcl, ncl, ecl, extra)){
    for(int i = 0; i < ncl; i++){
      track->GetElectron(j,x,y,z,t,e,dx,dy,dz);
      double ycheat = std::max(y,1.2);
      driftline_e->DriftElectron(RndmGaussian(x,dT*sqrt(ycheat-1.2)),1.2,RndmGaussian(z,dT*sqrt(ycheat-1.2)),0);  
      driftline_e->GetEndPoint(xendpoint, yendpoint, dummy, dummy, status);
      if(yendpoint>0.5*gap && yendpoint<1.5*gap){   //Electron drifted to a sensing wire
        for(int i=0; i<gain; i++){
          double angle = RndmGaussian(0, 1.4);
          driftline_i->DriftIon(xendpoint + r*sin(angle), gap + r*cos(angle),0,0);
        }
      }
    }
  }
 
  vd->Plot(true,false);
#if 0 
  app.Run(kTRUE);
#endif
}
