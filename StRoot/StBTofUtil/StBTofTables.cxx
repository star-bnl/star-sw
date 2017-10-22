/*******************************************************************
 *
 * $Id: StBTofTables.cxx,v 1.4 2017/10/20 17:50:33 smirnovd Exp $
 *
 *****************************************************************
 *
 * $Log: StBTofTables.cxx,v $
 * Revision 1.4  2017/10/20 17:50:33  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.3  2017/10/03 22:54:45  geurts
 * restore access to TOF status table (Calibrations/tof/tofStatus)
 *
 * Revision 1.2  2012/12/14 06:35:41  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.1  2009/08/28 17:22:11  dongx
 * first release
 *
 *
 *******************************************************************/
#include "StBTofTables.h"
#include "StDetectorDbMaker/St_tofStatusC.h"
//____________________________________________________________________                            
Int_t StBTofTables::status(Int_t trayId, Int_t moduleId, Int_t cellId) {
  static const Int_t mNModule = 32;
  static const Int_t mNCell = 6;
  Int_t i = (cellId -1) + mNCell*((moduleId-1) + mNModule*(trayId-1));
  return (Int_t) St_tofStatusC::instance()->status()[i];
}
