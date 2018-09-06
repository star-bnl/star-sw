#include <iostream>

#include <TCanvas.h>
#include <TROOT.h>
#include <TApplication.h>

#include "MediumMagboltz.hh"
#include "FundamentalConstants.hh"

using namespace Garfield;

int main(int argc, char * argv[]) {

  // TApplication app("app", &argc, argv);
 
  const double pressure = 3 * AtmosphericPressure;
  const double temperature = 293.15;
 
  // Setup the gas.
  MediumMagboltz* gas = new MediumMagboltz();
  gas->SetTemperature(temperature);
  gas->SetPressure(pressure);
  gas->SetComposition("Ar", 93., "CO2", 7.);
 
  // Set the field range to be covered by the gas table. 
  const int nFields = 20;
  const double emin =    100.;
  const double emax = 100000.;
  // Flag to request logarithmic spacing.
  const bool useLog = true;
  gas->SetFieldGrid(emin, emax, nFields, useLog); 

  const int ncoll = 10;
  // Switch on debugging to print the Magboltz output.
  gas->EnableDebugging();
  // Run Magboltz to generate the gas table.
  gas->GenerateGasTable(ncoll);
  gas->DisableDebugging();
  // Save the table. 
  gas->WriteGasFile("ar_93_co2_7.gas");

  // app.Run(kTRUE);

}
