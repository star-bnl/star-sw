/**********************************************************************
 *
 * $Id: StEStructMaxB.h,v 1.3 2005/09/14 17:14:24 msd Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  header to define #-events in mixing buffer. This information
 *               is now separated as it is needed in Support package
 *
 *               This has changed recently.  We needed to increase the size
 *               of the buffer to implement mixed-event delta N cut, so
 *               this head no longer defines the actual size of the buffer,
 *               but the number of mixing events we try to match with each event.
 *
 *
 ***********************************************************************/
#ifndef __STESTRUCTMAXB_HH
#define __STESTRUCTMAXB_HH

#define _MAXEBYEBUFFER_ 2

#endif
/***********************************************************************
 *
 * $Log: StEStructMaxB.h,v $
 * Revision 1.3  2005/09/14 17:14:24  msd
 * Large update, added new pair-cut system, added pair density plots for new analysis mode (4), added event mixing cuts (rewrote buffer for this)
 *
 * Revision 1.2  2005/03/28 22:59:08  porter
 * I opened a memory leak on last ci due to forgetting how StEStructBuffer
 * actually worked! This is now fixed with explaination in description.
 *
 * Revision 1.1  2005/03/03 01:30:44  porter
 * updated StEStruct2ptCorrelations to include pt-correlations and removed
 * old version of pt-correlations from chunhuih (StEStruct2ptPtNbar)
 *
 *
 *********************************************************************/
