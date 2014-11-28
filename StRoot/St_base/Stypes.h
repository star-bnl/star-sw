/*!
 * \file Stypes.h
 *
 * \author Valeri Fine  08/12/94
 *
 * Stypes
 * Basic types used by STAF - ROOT interface.                           
 *
 * This header file contains the set of the macro definitions           
 * to generate a ROOT dictionary for "pure" C-strucutre the way ROOT    
 * does it for the "normal" C++ classes                                 
 *                                                                      
 * This header file should be included into the all STAF table wrapper  
 * classes (by stic compiler)  
 * 
 * Note: while kStErr used to skip an event, this behavior
 * was changed in 2002 to only leave the current maker / sub-chain but
 * not the entire chain.
 *                                                                      
 */
#ifndef ST_NO_NAMESPACES
#ifdef __DECCXX
namespace std {};
#endif
using namespace std;
#endif

#ifndef STAR_Stypes
#define STAR_Stypes
 
#include <Ttypes.h>
#include "StTypeDefs.h"
#ifdef __HP_aCC
#include "St_HP_aCC.h"
#endif
/*!
 * \enum EReturnCodes 
 */
enum EReturnCodes {                                                    
  kStOK    = 0,                           /**< OK                                                 */
  kStOk    = 0,                           /**< OK                                                 */
  kStWarn  = 1,                           /**< Warning, something wrong but work can be continued */
  kStEOF   = 2,                           /**< End Of File                                        */
  kStErr   = 3,                           /**< Error, drop this i.e. leave the sub-chain          */
  kStERR   = 3,                           /**< Error, drop this i.e. leave the sub-chain          */
  kStFatal = 4,                           /**< Fatal error, processing impossible                 */
  kStFATAL = 4,                           /**< Fatal error, processing impossible                 */
  kStSKIP  = kStErr   + 10,               /**< Skip this event if chain allows                    */
  kStSkip  = kStSKIP,                     /**< Skip this event if chain allows                    */
  kStSTOP  = kStEOF   + 10,               /**< enum value kStSTOP                                 */
  kStStop  = kStSTOP                      /**< enum value kStStop                                 */
};  

#endif 
