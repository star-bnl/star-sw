/***************************************************************************
 *
 * $Id: StEmcSoftwareMonitor.cxx,v 2.1 1999/10/13 19:44:33 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcSoftwareMonitor.cxx,v $
 * Revision 2.1  1999/10/13 19:44:33  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:44:33  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StEmcSoftwareMonitor.h"
#include "tables/dst_mon_soft_emc.h"

static const char rcsid[] = "$Id: StEmcSoftwareMonitor.cxx,v 2.1 1999/10/13 19:44:33 ullrich Exp $";

ClassImp(StEmcSoftwareMonitor)

StEmcSoftwareMonitor::StEmcSoftwareMonitor()
{
    energy_emc = 0;
}

StEmcSoftwareMonitor::StEmcSoftwareMonitor(const dst_mon_soft_emc_st& mon)
{
    energy_emc = mon.energy_emc;
}

StEmcSoftwareMonitor::~StEmcSoftwareMonitor() {/* noop */}
