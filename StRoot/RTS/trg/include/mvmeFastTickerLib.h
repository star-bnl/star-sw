/*********************************************************************
 * $Id: mvmeFastTickerLib.h,v 1.1 2008/02/27 16:32:46 fine Exp $
 * $Revision: 1.1 $
 * $Date: 2008/02/27 16:32:46 $
 *
 *
 *  change log
 *---------------------------------------------------------------------
 * $Log: mvmeFastTickerLib.h,v $
 * Revision 1.1  2008/02/27 16:32:46  fine
 * build RTS repository trg
 *
 * Revision 1.1.1.1  2006/06/02 00:14:20  nelson
 * Top level for new trg_soft_dev respositry
 *
 * Revision 1.1.1.1  2003/09/18 21:35:40  kopytin
 * Cleaned up version trg_soft_sep1303, no cfg
 *
 * Revision 1.2  2000/02/18 19:09:01  levine
 * add ID field to all files
 *
 *
 *********************************************************************/
/*********************************************************************
 * $Id: mvmeFastTickerLib.h,v 1.1 2008/02/27 16:32:46 fine Exp $
 * $Revision: 1.1 $
 * $Date: 2008/02/27 16:32:46 $
 *
 *
 *  change log
 *---------------------------------------------------------------------
 * $Log: mvmeFastTickerLib.h,v $
 * Revision 1.1  2008/02/27 16:32:46  fine
 * build RTS repository trg
 *
 * Revision 1.1.1.1  2006/06/02 00:14:20  nelson
 * Top level for new trg_soft_dev respositry
 *
 * Revision 1.1.1.1  2003/09/18 21:35:40  kopytin
 * Cleaned up version trg_soft_sep1303, no cfg
 *
 * Revision 1.2  2000/02/18 19:09:01  levine
 * add ID field to all files
 *
 *
 *********************************************************************/
#ifndef _MVME_FAST_TICKER_LIB_H
#define _MVME_FAST_TICKER_LIB_H

#include <vxWorks.h>

extern void mvmeFastTickerInit(void) ;
extern UINT32 mvmeFastTickerGet(void) ;

#ifdef WILL_IMPLEMENT
extern void mvmeFastSleep(UINT32 microsec) ;
#endif

#endif 
