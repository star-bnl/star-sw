// $Id: StJetBEMCEnergyCut.cxx,v 1.1 2008/07/09 10:24:32 tai Exp $
#include "StJetBEMCEnergyCut.h"

#include <iostream>

using namespace std;

namespace StSpinJet {


StSpinJet::TowerEnergyDepositList StJetBEMCEnergyCut::Apply(const TowerEnergyDepositList &energyList)
{
  TowerEnergyDepositList ret;

  for(TowerEnergyDepositList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {

    if(!shouldKeep(*it)) continue;

    ret.push_back(*it);

  }

  return ret;
}


bool StJetBEMCEnergyCut::shouldKeep(const TowerEnergyDeposit& energyDeposit)
{

  if(mUse2003Cuts)
    if(!accept2003Tower(energyDeposit.towerId)) return false;

  if (mUse2005Cuts)
    if(energyDeposit.towerId > 2400) return false;

  if(energyDeposit.energy <= 0) return false;

  if(energyDeposit.status != 1) return false;

  if(energyDeposit.adc - energyDeposit.pedestal <= 0) return false;

  if((energyDeposit.adc - energyDeposit.pedestal) <= 2.*energyDeposit.rms) return false;

  return true;

}


bool StJetBEMCEnergyCut::accept2003Tower(int id)
{
    if( id==555
	|| id==615
	|| id==656
	|| id==772
	|| id==1046
	|| id==1048
	|| id==1408
	|| id==1555
	|| id==1750
	|| id==1773
	|| id==2073
	|| id==2093
	|| id==2096
	|| (id>=1866 && id<=1894)
	|| id==511
	|| id==1614
	|| id==1615
	|| id==1616
	|| id==1636
	|| id==1899
	|| id==2127
	|| id==953
	|| id==1418
	|| id==1419
	|| id==1878
	|| id==1879
	|| id==1881
	|| (id>=1042 && id<=1045)
	|| (id>=1385 && id<=1387)
	|| (id>=1705 && id<=1708)
	|| (id>=1725 && id<=1728)
	|| (id>=1745 && id<=1748)
	|| (id>=1765 && id<=1768)
	|| (id>=1785 && id<=1788)
	|| id>2400
	)
	{
	    return false;
	}
    else {
	return true;
    }
}



}
