#ifndef HistogramGroup_H_Included 
#define HistogramGroup_H_Included 
#include "Sti/Base/Named.h"
#include "Sti/Base/Described.h"
#include "Sti/Base/Vectorized.h"
#include <TH1.h>
#include <TH2.h>
#include <TFile.h>

class HistogramGroup : public Named, public Described, public Vectorized<TH1>
{
 public: 
  HistogramGroup();
  HistogramGroup(const string & name, const string & description);
  ~HistogramGroup();
  void write(TFile * file);
  void write(const string & fileName, const string &option="RECREATE");  
  void write();
  void reset();
  TH1D * HistogramGroup::book(const string &title, 
			      const string & description, 
			      int n, 
			      double xMin,
			      double xMax);
  TH2D * HistogramGroup::book(const string &title, 
			      const string & description, 
			      int nx, 
			      double xMin,
			      double xMax,
			      int ny, 
			      double yMin,
			      double yMax);

};


#endif
