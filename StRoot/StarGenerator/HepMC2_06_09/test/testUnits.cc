//
// Test Units
//
#include <iostream>

#include "HepMC/Units.h"

int main()
{
  
  int err = 0;
  double cf;

  std::cout << "Default units: " << HepMC::Units::name(HepMC::Units::default_momentum_unit())
     << " " << HepMC::Units::name(HepMC::Units::default_length_unit()) << std::endl;

  // check momentum conversion factors
  cf = conversion_factor( HepMC::Units::GEV, HepMC::Units::GEV );
  if( cf != 1 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf 
                << " for GEV to GEV - should be 1 \n";
  }
  cf = conversion_factor( HepMC::Units::MEV, HepMC::Units::MEV );
  if( cf != 1 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf
                << " for MEV to MEV - should be 1 \n";
  }
  cf = conversion_factor( HepMC::Units::MEV, HepMC::Units::GEV );
  if( cf != 0.001 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf
                << " for MEV to GEV - should be 0.001 \n";
  }
  cf = conversion_factor( HepMC::Units::GEV, HepMC::Units::MEV );
  if( cf != 1000.0 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf
                << " for GEV to MEV - should be 1000 \n";
  }

  // check length conversion factors
  cf = conversion_factor( HepMC::Units::MM, HepMC::Units::MM );
  if( cf != 1 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf
                << " for MM to MM - should be 1 \n";
  }
  cf = conversion_factor( HepMC::Units::CM, HepMC::Units::CM );
  if( cf != 1 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf
                << " for CM to CM - should be 1 \n";
  }
  cf = conversion_factor( HepMC::Units::CM, HepMC::Units::MM );
  if( cf != 10.0 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf
                << " for CM to MM - should be 10 \n";
  }
  cf = conversion_factor( HepMC::Units::MM, HepMC::Units::CM );
  if( cf != 0.1 ) {
      ++err;
      std::cerr << "wrong conversion factor " << cf
                << " for MM to CM - should be 0.1 \n";
  }

  return err;
}
