 //
// Grant Webb <grant.webb@uky.edu>
// University of Kentucky 
// 6 March 2012
//

#include "StjTowerEnergyFraction.h"

ClassImp(StjTowerEnergyFraction);

StjTowerEnergyList StjTowerEnergyFraction::Do( const StjTowerEnergyList& energyDepositList)
{
  StjTowerEnergyList elist;
  // Tower loop
  for (StjTowerEnergyList::const_iterator iTower = energyDepositList.begin(); iTower != energyDepositList.end(); ++iTower) {

    StjTowerEnergy tower = *iTower;
    // Add a certain fraction to the tower energy
    tower.energy += mFraction *tower.energy;
    elist.push_back(tower);
  } // End tower loop  
  return elist;
}
