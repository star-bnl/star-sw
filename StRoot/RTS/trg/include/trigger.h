/*
 * Header Name: trigger.h                                                             
 * Header Number: x.y                                                                  
 * Package Name: All                                                                   
 * Created: E. G. Judd 05/21/98                                                        
 * Description: Global trigger header file.                                            
 *              Contains definitions of global variables, queue names etc...           
 * History:                                                                            
 *                                                                                     
 *     09Jun98 JMN:  Added SEND_DAQ command.                                           
 *                   Added dummy word in event descriptor to ensure length is modulo 8 
 *                    because of DMA restrictions in DAQ crate                         
 *                   Added explicit payload array in message packet                    
 *     29Jan99 EGJ:  Changed MAX_RAW_DATA_BLOCKS to 11 because npre=npost=5 max        
 *                   Changed TRG_DATA_LEN to 400, not including EMC right now          
 *                   Added L1_DATA_LEN and TRG_EVT_LEN                                 
 *                   Updated MSGQLEN to 4095=maximum number of tokens we can have      
 *                   Added SCL1, SCDC and SCL0 to list of tasks                        
 *                   Added ADD_TOKEN, CONFIG, DONE_CONFIG to list of commands          
 *     28Sep99 zm    Changed DAQ messageQ id to 16                                     
 *     28Sep99 JMN:  Added fmt_event_desc structure and changed defintion of           
 *                   EV_DESC_LEN, TRG_SUM_LEN, TRG_DATA_LEN                            
 *     29Sep99 zm    Major cleaning - new structures and new macros                    
 *     22Nov99 zm    Deleted ActionWd_type; changed raw data from integers to bytes    
 *     03Dec99 EGJ   Updated L0 DSM data structure                                     
 *     03Dec99 zm/ej Updated structures to system-wide consistent form                 
 *     06dec99 zm    Removed trigger structures into trgStructures.h and included file 
 *     26jan00 zm    included token.h header file                                      
 *     28mar00 zm    redefined trgState enum variables                                 
 *     18Apr00 zm    added hostLib                                                     
 *     25Feb01 JMN   redesigned                                                        
 *     18Jun01 zm    added a few parameters for writing to L2 memory                   
 *     10Oct01 zm    added UPC definition  
 *     02Dec02 zm    made compatible with linux
 *     07Dec02 JMN   add FORCING to trgState enums
 */

#ifndef TRIGGER_H
#define TRIGGER_H

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <syslog.h>

#ifndef LITTLE_ENDIAN  /* do not include if linux */

#include <iv.h>
#include <ioLib.h>
#include <taskLib.h>
#include <vxWorks.h>
#include <msgQLib.h>
#include <semLib.h>
#include <wdLib.h>
#include <intLib.h>
#include <sysLib.h>
#include <logLib.h>
#include <hostLib.h>

#endif

#define MAX_TOKEN     4095              /* Maximum number of tokens allowed in the system */
#define MAX_PATH_LEN   132

#define DC_PMC_MEM_BASE 0x30000000      /* DC extended memory location */
#define L2_PMC_MEM_BASE 0x30000000      /* L2 extended memory location */

#define L2_WRITE_MEM 0x31900000         /* area in l2 memory for writing l2 data */
#define MAX_EVENT_WRITE 10000           /* maximum events for memory */

#define FATAL_ERROR  -2
#define OK            0
#define SUCCESS       0

#define UPC_TW_BITMASK 0x3000

typedef int STATUS;
typedef int FLAG;

typedef enum {
  DISABLED,
  ENABLED,
  CONFIGURING,
  READY,
  RUNNING,
  PAUSED,
  STOPPING,
  STOPPED,
  PAUSING,
  FORCING
} trgState;

typedef unsigned int uint;              /* Several shortcut definitions */
typedef unsigned short ushort;
typedef unsigned long  ulong;
typedef unsigned char BYTE;
typedef unsigned int  WORD;


#endif
