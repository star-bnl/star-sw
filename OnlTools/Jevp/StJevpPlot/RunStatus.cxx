#include "RunStatus.h"
#include <rtsLog.h>

ClassImp(RunStatus) ;

void RunStatus::dump()
{
  LOG("JEFF","Run Status----------------");
  LOG("JEFF", "run=%d",mRun);
  LOG("JEFF", "End=%d",mEnd);
  LOG("JEFF", "Trigger Bits=0x%x",mTriggerBitsRun);
  LOG("JEFF", "Detector Bits=0x%x",mDetectorBitsRun);
  LOG("JEFF", "--------------------------");
}



/***************************************************************************
 *
 * $Id: RunStatus.cxx,v 1.1 2010/12/07 14:25:22 jml Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: RunStatus.cxx,v $
 * Revision 1.1  2010/12/07 14:25:22  jml
 * adding Jevp
 *
 * Revision 1.2  2010/05/12 17:41:20  jml
 * various...
 *
 * Revision 1.1  2010/03/29 15:16:16  jml
 * more changes..
 *
 * Revision 1.1  2010/03/19 17:58:14  jml
 * modify to scripted version, add hlt
 *
 * Revision 1.1  2009/10/20 20:38:09  jml
 * getting up to date...
 *
 * Revision 1.1  2009/01/23 16:11:06  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.1  2007/02/27 15:23:39  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

