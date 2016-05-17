#include <iostream>
#include <TCanvas.h>
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

using namespace Garfield;

double transfer(double t){
  const double tau = 160;
  const double fC_to_mV = 12.7;
  return fC_to_mV*exp(4)*pow((t/tau),4)*exp(-4*t/tau);  //impulse response of the PASA

}


int main(int argc, char * argv[]) {

  TApplication app("app", &argc, argv);
  
  // Switch between IROC and OROC
  const bool iroc = false;

  //gating ON/OFF
  const bool gating = true;

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
  ComponentAnalyticField* comp_i = new ComponentAnalyticField();
  comp_i->SetPeriodicityX(nRep * period);
  for (int i = 0; i < nRep+10; ++i) {
   comp_i->AddWire((i - 2.) * period, ys, dSens, vSens, "s", 100., 50., 19.3, 1);
  }
  for (int i = 0; i < nRep +5; ++i) {
    comp_i->AddWire(dc * (i + 0.5), yc, dCath, 0., "c", 100., 50., 19.3, 1);
  }
  if (gating){
    for (int i = 0; i < nRep * 2; i=i+2)
      comp_i->AddWire(dg * (i + 0.5), yg, dGate, vGate+deltav, "g+", 100., 50., 19.3, 1);
    for (int i = 1; i < nRep * 2; i=i+2)
      comp_i->AddWire(dg * (i + 0.5), yg, dGate, vGate-deltav, "g-", 100., 50., 19.3, 1);
  }
  else{
    for (int i = 0; i < nRep * 7; ++i) {
      comp_i->AddWire(dg * (i + 0.5), yg, dGate, vGate, "g", 100., 50., 19.3, 1);  //set trap radius to 1 to avoid stopping the drift line prematurely at the gating

    }
  }
  comp_i->AddPlaneY(0., 0., "pad_plane");
  comp_i->AddPlaneY(yHV, vHV, "HV");

  comp_i->AddReadout("pad_plane");
 
  //Add magnetic field
  comp_i->SetMagneticField(0,0.5,0);
  comp_i->SetGeometry(geo);

  // Make a sensor for ions (gating closed)
  Sensor* sensor_i = new Sensor();
  sensor_i->AddComponent(comp_i);
  sensor_i->AddElectrode(comp_i, "pad_plane");
  sensor_i->SetTimeWindow(0.,1,50000); // can be changed for less/better resolution in time (effect on convolution can be important)
 
  // Plot isopotential contours
  ViewField* fView = new ViewField;
  fView->SetSensor(sensor_i);
  fView->SetArea(-nRep * period / 2., 0.,
                  nRep * period / 2., 2);
  fView->SetVoltageRange(-400., 1000.);
  fView->PlotContour();
 
  //Plot drift line
  DriftLineRKF* driftline = new DriftLineRKF();
  //driftline->EnableDebugging();
  //driftline->EnableVerbose();
  driftline->SetSensor(sensor_i);
 
  const double gain = 10000;
  double r = 0.003;
  double dummy = 0.;
  std::string status="";
  double endpoint = gap;
  int plane = 0, cathode= 0, gate = 0, escape = 0; // used to store number of ions that drift respectively to plane, cathode, gate or drift vol
  for(int i=0; i<gain; i++){
    double angle = RndmGaussian(0, 1.4);
    driftline->DriftIon(r*sin(angle), gap + r*cos(angle),0,0);
    driftline->GetEndPoint(dummy,endpoint,dummy,dummy,status);
    if(endpoint < gap/2) //Drifted to plane
      plane++;
    else if(endpoint>1.5*gap && endpoint<2.5*gap)  //drifted to cathode wires
      cathode++;
    else if(endpoint>2.5*gap && endpoint<2*gap+0.3*1.5) //drifted to gating wires
      gate++;
    else  //escaped outside gas chamber
      escape++;
  }
 
  // Plot signal
  ViewSignal* vs1 = new ViewSignal();
  vs1->SetSensor(sensor_i);
  vs1->PlotSignal("pad_plane");
  ViewSignal* vs2 = new ViewSignal();
  sensor_i->SetTransferFunction(transfer);
  sensor_i->ConvoluteSignal();
  vs2->SetSensor(sensor_i);
  vs2->PlotSignal("pad_plane");
 
  app.Run(kTRUE);

}

