/***************************************************************************
 *
 * $Id: StTpcUnpacker.hh,v 1.1 1999/02/19 16:27:48 fisyak Exp $
 *
 * Author: bl, Jan 20, 1999
 *         based on design outlined in:
 *         DAQ interface to Offline 12/30/98 (M. Levine, M. Shultz, B. Lasiuk)
 ***************************************************************************
 *
 * Description: An abstract class which defines an interface which
 *              allows access to raw data.  Is used by the
 *              TRS and DAQ readers!
 *
 * Return codes:
 * --> Data Access:
 *    int getSector()
 *     0 = OK
 *    -1 = wrong sector number
 *    -2 = wrong event
 * --> int getSequences()
 *     0 = OK
 *    -1 = wrong Row
 *    -2 = wrong Pad
 *    -3 = wrong *nSeq
 *    -4 = wrong StSequence
 *    -1000 = no call to get Sector
 * --> int getPadList()
 *    >0 = no sequences on pad
 *    -1000 = getSector() call failed
 *
 ***************************************************************************
 *
 * $Log: StTpcUnpacker.hh,v $
 * Revision 1.1  1999/02/19 16:27:48  fisyak
 * Add from StarClassLibary
 *
 * Revision 1.5  1999/02/10 04:30:53  lasiuk
 * put TObject as base class
 *
 * Revision 1.4  1999/02/04 04:01:33  lasiuk
 * virtual destructor code
 *
 * Revision 1.3  1999/02/03 01:15:34  lasiuk
 * virtual destructor
 *
 * Revision 1.2  1999/02/02 23:50:11  lasiuk
 * name
 *
 * Revision 1.1  1999/02/02 23:36:34  lasiuk
 * Initial Revision/rename
 *
 **************************************************************************/
#ifndef ST_TPC_UNPACKER_HH
#define ST_TPC_UNPACKER_HH

#include "TObject.h"

struct StSequence;
class  StTpcRawDataEvent;

#ifndef __ROOT__
class StTpcUnpacker
#else
class StTpcUnpacker : public TObject
#endif
{
public:
    virtual ~StTpcUnpacker() {};
    
    virtual int  getSector(int which, StTpcRawDataEvent* event) = 0;
    virtual int  getSequences(int padRow, int pad, int *nSeq, StSequence** seq)= 0;
    virtual int  getPadList(int padRow, unsigned char **padList) = 0;
    virtual void clear() = 0;
};
#endif
