#ifndef MultiGraph_h
#define MultiGraph_h

#include <string>

#include "TH1F.h"
#include "TMultiGraph.h"

namespace rh {

class MultiGraph : public TMultiGraph
{
public:

   MultiGraph();
   MultiGraph(std::string name, std::string title);

   void SetHistogram(TH1F* h);
   virtual void Draw(Option_t* chopt = "A");

   ClassDef(MultiGraph, 1)
};

}

#endif
