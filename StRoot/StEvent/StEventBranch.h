/*!
 * \class StEventBranch 
 * \author Victor Perev, May 2001
 */
/***************************************************************************
 *
 * $Id: StEventBranch.h,v 2.2 2002/02/22 22:56:47 jeromel Exp $
 *
 * Author: Victor Perev, May 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************/
#ifndef StEventBranch_hh
#define StEventBranch_hh
#include "StObject.h"

class StEvent;

class StEventBranch : public StXRef {
public:
         StEventBranch(const char *brName="", StEvent *evt=0, UInt_t tally=0);
virtual ~StEventBranch(){};
virtual	 StXRefMain *MakeMain();
         void        AddKlass(const char* className);
virtual  void        Synchro(int toMain);

ClassDef(StEventBranch,1)
};
#endif







