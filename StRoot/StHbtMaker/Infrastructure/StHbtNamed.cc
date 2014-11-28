/***************************************************************************
 *
 * $Id: 
 *
 * Author:  Laurent Conin, Fabrice Retiere, Subatech, France
 ****************************************************************************
 * Description:  implementation of StHbtNamed 
 *
 ***************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "StHbtMaker/Infrastructure/StHbtNamed.hh"

#ifdef __ROOT2__
  ClassImp(StHbtNamed)
#endif

StHbtNamed::StHbtNamed()  : mName(new char[1]) {mName[0]=0;};

StHbtNamed::StHbtNamed(const char * aName) { 
 SetName(aName);
};

StHbtNamed::StHbtNamed(const StHbtNamed& aNamed) { SetName(aNamed.GetName()); }

void StHbtNamed::SetName(const char * aName) { 
  if (mName) delete[] mName;
  if (aName) {
    mName=new char[strlen(aName)+1];
    strcpy(mName,aName);
  } else {
    mName=new char[1];
    mName[0]=0;
  };
};
