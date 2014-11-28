/*****************************************************************
 * $Id: StMuPmdUtil.h,v 1.1 2004/04/02 03:36:21 jeromel Exp $
 *
 * Class : StMuPmdUtil
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the utility class for PMD to convert 
 *              StEvent to StMuDst and vice versa
 * ****************************************************************
 * $Log: StMuPmdUtil.h,v $
 * Revision 1.1  2004/04/02 03:36:21  jeromel
 * New files for PMD
 *
 * ****************************************************************/
#ifndef StMuPmdUtil_h
#define StMuPmdUtil_h
#include "TObject.h"

class StMuPmdCollection;
class StPhmdCollection;

class StMuPmdUtil : public TObject
{
  protected:

  public:
	  StMuPmdUtil();
	  ~StMuPmdUtil();
	  StMuPmdCollection* getMuPmd(StPhmdCollection*);
	  StPhmdCollection*  getPmd(StMuPmdCollection*);

	  void fillMuPmd(StPhmdCollection*, StMuPmdCollection*);
	  void fillPmd(StMuPmdCollection*, StPhmdCollection*);

  ClassDef(StMuPmdUtil,1);
};
#endif

