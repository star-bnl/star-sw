/***************************************************************************    
 *    
 * $Id: Centrality.cc,v 1.1 2002/04/02 20:05:16 jklay Exp $
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
 * $Log: Centrality.cc,v $
 * Revision 1.1  2002/04/02 20:05:16  jklay
 * Bums analysis tools for highpt uDSTs
 *  
 *   
 **************************************************************************/  
#include "Centrality.h"

NchCentrality centrality(double zdcsum, double ctbevt){
    if (zdcsum<66 && ctbevt>8500)
	return kFive;
    else if (zdcsum<91  && zdcsum>=66 && ctbevt>5000)
	return kTen;
    else if (zdcsum<120 && zdcsum>=91 && ctbevt>2150)
	return kTwenty;
    else if (zdcsum<(120+.005*(ctbevt-2151)) && zdcsum>=120 && ctbevt>2150)
	return kThirty;
    else if (zdcsum<(120+.022*(ctbevt-2151)) && zdcsum>=(120+.005*(ctbevt-2151)) && ctbevt>2150)
	return kForty;
    else if (zdcsum>=(120+.022*(ctbevt-2151)) && zdcsum>(120-.125*(ctbevt-2150)))
	return kFifty;
    else if (zdcsum>(120-.0271*(ctbevt-2151)) && zdcsum<=(120-.125*(ctbevt-2150)))
	return kSixty;
    else if (zdcsum>(120-.0142*(ctbevt-2151)) && zdcsum<=(120-.0271*(ctbevt-2151)))
	return kSeventy;
    else if (zdcsum>(120-.0005*(ctbevt-2151)) && zdcsum<=(120-.0142*(ctbevt-2151)))
	return kEighty;
    else return kTotal;
}

NchCentrality centrality(int nHMinus)
{
  if(nHMinus>=212) return kFive;
  else if(nHMinus>=179) return kTen;
  else if(nHMinus>=127) return kTwenty;
  else if(nHMinus>=91) return kThirty;
  else if(nHMinus>=57) return kForty;
  else if(nHMinus>=35) return kFifty;
  else if(nHMinus>=20) return kSixty;
  else if(nHMinus>=10) return kSeventy;
  else if(nHMinus>=4) return kEighty;
  else return kTotal;

}
