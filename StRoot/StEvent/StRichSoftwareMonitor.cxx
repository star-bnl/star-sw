/***************************************************************************
 *
 * $Id: StRichSoftwareMonitor.cxx,v 2.2 1999/10/28 22:26:24 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichSoftwareMonitor.cxx,v $
 * Revision 2.2  1999/10/28 22:26:24  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:07  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StRichSoftwareMonitor.h"
#include "tables/St_dst_mon_soft_rich_Table.h"

static const char rcsid[] = "$Id: StRichSoftwareMonitor.cxx,v 2.2 1999/10/28 22:26:24 ullrich Exp $";

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
