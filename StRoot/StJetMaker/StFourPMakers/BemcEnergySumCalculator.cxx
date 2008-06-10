// $Id: BemcEnergySumCalculator.cxx,v 1.1 2008/06/10 05:40:38 tai Exp $
#include "BemcEnergySumCalculator.h"

#include "CollectEnergyDepositsFromBEMC.h"

namespace StSpinJet {

BemcEnergySumCalculator::BemcEnergySumCalculator(CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC)
 : _collectEnergyDepositsFromBEMC(collectEnergyDepositsFromBEMC)
{

 }

}
