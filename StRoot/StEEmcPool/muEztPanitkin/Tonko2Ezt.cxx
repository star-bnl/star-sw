// $Id: Tonko2Ezt.cxx,v 1.2 2008/12/19 17:59:27 fine Exp $
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <TObjArray.h>

#include "Tonko2Ezt.h"

#ifdef IN_PANITKIN
// this class is implemented only in online-mode
#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include "emcReader.h"
#else
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_EMC/emcReader.h"
#endif

//-------------------------------------------
//-------------------------------------------
 Tonko2Ezt:: Tonko2Ezt() {
   // printf("\n\n  Tonko2Ezt:: Tonko2Ezt() \n\n");
   //   printf("ETOW data in %d \n",emc.etow_in);
   // printf("ESMD data in %d \n",emc.esmd_in);  

   if(emc.etow_in) {     // ETOW data
     int ib;
     for(ib=0;ib<ETOW_MAXFEE;ib++) {
       eETow.createBank(ib,ETOW_PRESIZE,128);
       eETow.setHeader(ib,emc.etow_pre[ib]);
       eETow.setData(ib,emc.etow[ib]);
     }
     // printf("TTTTTTTTTTTTTTTTTTTT\n\n\n");
     // eETow.print(0);
   }// end of ETOW 

   if(emc.esmd_in) {   // ESMD data
    
     int ib;
     for(ib=0;ib<ESMD_MAXFEE;ib++) {
       eESmd.createBank(ib,ESMD_PRESIZE,ESMD_DATSIZE);
       eESmd.setHeader(ib,emc.esmd_pre[ib]);
       eESmd.setData(ib,emc.esmd[ib]);
       //eESmd.print(ib,0);
#if 0
       printf("%x %x %x %x \n", emc.esmd_pre[ib][0], emc.esmd_pre[ib][1], emc.esmd_pre[ib][2], emc.esmd_pre[ib][3]);
#endif    
     }
     // printf("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ\n\n\n");
   } 
   //  exit(1);
} // end of ESMD
#endif
// $Log: Tonko2Ezt.cxx,v $
// Revision 1.2  2008/12/19 17:59:27  fine
// Add NEW_DAQ_READER flag
//
// Revision 1.1  2005/04/28 20:54:47  balewski
// start
//

  
