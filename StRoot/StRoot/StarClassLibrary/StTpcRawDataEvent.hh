/***************************************************************************
 *
 * $Id: StTpcRawDataEvent.hh,v 1.2 2008/06/20 15:23:15 fisyak Exp $
 *
 * Author: bl, Jan 20, 1999
 *         based on design outlined in:
 *         DAQ interface to Offline 12/30/98 (M. Levine, M. Shultz, B. Lasiuk)
 ***************************************************************************
 *
 * Description: An abstract class which encapsulates the data of a single
 *              event.
 *
 ***************************************************************************
 *
 * $Log: StTpcRawDataEvent.hh,v $
 * Revision 1.2  2008/06/20 15:23:15  fisyak
 * Unified TpcRaw data
 *
 * Revision 1.1  1999/02/19 16:27:48  fisyak
 * Add from StarClassLibary
 *
 * Revision 1.4  1999/02/10 04:30:53  lasiuk
 * put TObject as base class
 *
 * Revision 1.3  1999/02/03 03:21:33  lasiuk
 * {} for virtual destructor
 *
 * Revision 1.2  1999/02/03 01:24:03  lasiuk
 * virtual destructor
 *
 * Revision 1.1  1999/02/01 22:33:39  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TPC_RAW_DATA_EVENT_HH
#define ST_TPC_RAW_DATA_EVENT_HH
#include "StTpcRawData.h"
typedef StTpcRawData StTpcRawDataEvent;
#endif
