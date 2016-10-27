
#include "MultiGraph.h"

#include "TH1F.h"
#include "TList.h"
#include "TGraph.h"
#include "TPad.h"
#include "TPaveStats.h"


ClassImp(rh::MultiGraph)

namespace rh {

using namespace std;

MultiGraph::MultiGraph() : TMultiGraph()
{
}

MultiGraph::MultiGraph(string name, string title) : TMultiGraph(name.c_str(), title.c_str())
{
}


void MultiGraph::SetHistogram(TH1F* h)
{
   fHistogram = h;
}


void MultiGraph::Draw(Option_t* chopt)
{
   // First check if this container is not empty
   TList* graphs = GetListOfGraphs();

   if (!graphs || graphs->GetSize() < 1) return;

   if (fHistogram) {
      fHistogram->SetStats(kFALSE);
      fHistogram->Draw();
      TMultiGraph::Draw(chopt);
   }

   gPad->Update();

   TIter  next(graphs);
   Double_t height = graphs->GetSize() > 5 ? 0.95/graphs->GetSize() : 0.18;
   UShort_t iStat = 0;

   while ( TGraph *iGraph = (TGraph*) next() )
   {
      if ( iGraph->GetN() <= 0) continue;

      TPaveStats *stats = (TPaveStats*) iGraph->FindObject("stats");

      if (stats) {
         stats->SetLineColor(iGraph->GetMarkerColor());
         stats->SetLineWidth(2);
         stats->SetX1NDC(0.80);
         stats->SetY1NDC(0.99 - height*(iStat+1) );
         stats->SetX2NDC(0.99);
         stats->SetY2NDC(0.99 - iStat*height);

         iStat++;
      }
   }
}

}
