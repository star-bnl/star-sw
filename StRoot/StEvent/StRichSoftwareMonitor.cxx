/***************************************************************************
 *
 * $Id: StRichSoftwareMonitor.cxx,v 2.1 1999/10/13 19:45:07 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichSoftwareMonitor.cxx,v $
 * Revision 2.1  1999/10/13 19:45:07  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:45:07  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StRichSoftwareMonitor.h"
#include "tables/dst_mon_soft_rich.h"

static const char rcsid[] = "$Id: StRichSoftwareMonitor.cxx,v 2.1 1999/10/13 19:45:07 ullrich Exp $";

ClassImp(StRichSoftwareMonitor)

StRichSoftwareMonitor::StRichSoftwareMonitor()
{
    mult_rich_tot = 0;
}

StRichSoftwareMonitor::StRichSoftwareMonitor(const dst_mon_soft_rich_st& mon)
{
    mult_rich_tot = mon.mult_rich_tot;
}

StRichSoftwareMonitor::~StRichSoftwareMonitor() {/* noop */}
