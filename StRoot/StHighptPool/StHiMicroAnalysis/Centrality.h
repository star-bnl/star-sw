/***************************************************************************  
 *  
 * $Id: Centrality.h,v 1.1 2002/04/02 20:05:16 jklay Exp $  
 *  
 * Author: Bum Choi, UT Austin, Apr 2002  
 *  
 ***************************************************************************  
 *  
 * Description:  Manuel's zdc-ctb, Year 1 centrality definitions
 *               
 *               
 ***************************************************************************
 *
 * $Log: Centrality.h,v $
 * Revision 1.1  2002/04/02 20:05:16  jklay
 * Bums analysis tools for highpt uDSTs
 *
 * 
 **************************************************************************/
/*
  manuel's zdc-ctb, centrality definitions

 */

#ifndef Centrality_hh
#define Centrality_hh

enum NchCentrality {kFive, kTen, kTwenty,
		    kThirty, kForty, kFifty,
		    kSixty, kSeventy, kEighty, kTotal, KNull};

NchCentrality centrality(double zdcsum, double cdbevt);

NchCentrality centrality(int nHMinus);

#endif
