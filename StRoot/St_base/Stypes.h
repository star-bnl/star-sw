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
 */

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
  kStOK    = 0,                           /**< OK     */
  kStOk    = 0,                           /**< OK     */
  kStWarn  = 1,                           /**< Warning, something wrong but work can be continued */
  kStEOF   = 2,                           /**< End Of File   */
  kStErr   = 3,                           /**< Error, drop this and go to the next event    */
  kStERR   = 3,                           /**< Error, drop this and go to the next event   */
  kStFatal = 4,                           /**< Fatal error, processing impossible  */
  kStFATAL = 4,                           /**< Fatal error, processing impossible  */
  kStSKIP  = kStErr   + 10,               /**< enum value kStSKIP   */
  kStSkip  = kStSKIP,                     /**< enum value kStSkip   */
  kStSTOP  = kStFATAL + 10,               /**< enum value kStSTOP   */
  kStStop  = kStSTOP                      /**< enum value kStStop   */
};  

#endif 
