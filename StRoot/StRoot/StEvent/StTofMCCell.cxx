/***************************************************************************
 *
 * $Id: StTofMCCell.cxx,v 2.2 2003/09/02 17:58:05 perev Exp $
 *
 * Author: F. Geurts, May 2003
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCCell.cxx,v $
 * Revision 2.2  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.1  2003/05/21 18:24:20  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StTofMCCell.h"

static const char rcsid[] = "$Id: StTofMCCell.cxx,v 2.2 2003/09/02 17:58:05 perev Exp $";

ClassImp(StTofMCCell)

StTofMCCell::StTofMCCell() { /* nopt */ }

StTofMCCell::StTofMCCell(const StTofMCInfo& MCInfo) :
    mTofMCInfo(MCInfo) { /* nopt */ }

StTofMCCell::~StTofMCCell() { /* noop */ }
    
int
StTofMCCell::operator==(const StTofMCCell& MCCell) const
{
    return (MCCell.mTrayIndex == mTrayIndex &&
	    MCCell.mModuleIndex == mModuleIndex &&
	    MCCell.mCellIndex == mCellIndex && MCCell.mTofMCInfo == mTofMCInfo);
}

int
StTofMCCell::operator!=(const StTofMCCell& MCCell) const
{
    return !(*this == MCCell);  // use operator==()
}

ostream&
operator<<(ostream& os, const StTofMCCell& cell)
{
    return (os << "StTofMCCell::> "  << ", tray= " << cell.trayIndex()
	    << ", module= " << cell.moduleIndex() << ", cell= "
	    << cell.cellIndex() << ", adc= " << cell.adc()  
	    << ", tdc= " << cell.tdc() << endl << "MCInfo:  " << cell.mcInfo());
}
