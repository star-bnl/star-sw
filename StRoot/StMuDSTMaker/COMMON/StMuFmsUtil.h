/***************************************************************************
 *
 * $Id: StMuFmsUtil.h,v 1.1 2010/01/25 03:57:39 tone421 Exp $
 *
 * Author: Jingguo Ma, Jan 2010
 ***************************************************************************
 *
 * Description: FMS Util to convert between StEvent and MuDst
 *
 ***************************************************************************
 *
 * $Log: StMuFmsUtil.h,v $
 * Revision 1.1  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 **************************************************************************/
#ifndef StMuFmsUtil_h
#define StMuFmsUtil_h
#include "TObject.h"

class StMuFmsCollection;
class StFmsCollection;

class StMuFmsUtil : public TObject
{
  protected:
    
  public:
                       StMuFmsUtil();
                       ~StMuFmsUtil();
    StMuFmsCollection* getMuFms(StFmsCollection*);
    StFmsCollection*   getFms(StMuFmsCollection*);
    void               fillMuFms(StMuFmsCollection*,StFmsCollection*);
    void               fillFms(StFmsCollection*,StMuFmsCollection*);
              
  ClassDef(StMuFmsUtil,1)
};

#endif
