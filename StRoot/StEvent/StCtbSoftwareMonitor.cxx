/***************************************************************************
 *
 * $Id: StCtbSoftwareMonitor.cxx,v 2.1 1999/10/13 19:44:26 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbSoftwareMonitor.cxx,v $
 * Revision 2.1  1999/10/13 19:44:26  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:44:26  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StCtbSoftwareMonitor.h"
#include "tables/dst_mon_soft_ctb.h"

static const char rcsid[] = "$Id: StCtbSoftwareMonitor.cxx,v 2.1 1999/10/13 19:44:26 ullrich Exp $";

ClassImp(StCtbSoftwareMonitor)

StCtbSoftwareMonitor::StCtbSoftwareMonitor()
{
    mult_ctb_tot = 0;
}

StCtbSoftwareMonitor::StCtbSoftwareMonitor(const dst_mon_soft_ctb_st& mon)
{
    mult_ctb_tot = mon.mult_ctb_tot;
}

StCtbSoftwareMonitor::~StCtbSoftwareMonitor() {/* noop */}
