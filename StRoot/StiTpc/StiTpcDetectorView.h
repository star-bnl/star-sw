#if !defined(StiTpcDetectorView_H_INCLUDED)
#define StiTpcDetectorView_H_INCLUDED

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <string>
using std::string;

#include "Sti/StiDetectorView.h"

/*! 
\class StiTpcDetectorView
This class the views used for the TPC.

\author Claude A Pruneau
*/

class StiTpcDetectorView : public StiDetectorView 
{
public:

  StiTpcDetectorView(const string & name);
  virtual ~StiTpcDetectorView();

  virtual void setDefault();
  virtual void setFullView();
  virtual void setSkeletonView();

};



