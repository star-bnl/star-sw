#ifndef HistogramGroup_H_Included 
#define HistogramGroup_H_Included 
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/Base/Vectorized.h"
#include <TH1.h>

class HistogramGroup : public Named, public Described, public Vectorized<TH1>
{
 public: 
  HistogramGroup();
  HistogramGroup(const string & name, const string & description);
  ~HistogramGroup();
  void write();
  void reset();
};


#endif
