/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/

#include "VertexHContainer.h"

#include "TF1.h"

#include "VecBosEvent.h"


ClassImp(VertexHContainer)

using namespace std;


/** Default constructor. */
VertexHContainer::VertexHContainer() : PlotHelper()
{
   BookHists();
}


VertexHContainer::VertexHContainer(TDirectory *dir) : PlotHelper(dir)
{
   BookHists();
}


/** Default destructor. */
VertexHContainer::~VertexHContainer()
{
}


/** */
void VertexHContainer::BookHists()
{
   string shName;
   TH1*   hist;

   fDir->cd();

   o["hVertexId"] = hist = new TH1I("hVertexId", "; Vertex Id; Num. of Vertices", 10, 0, 10);
   hist->SetOption("hist GRIDX GRIDY");

   o["hVertexRank"] = hist = new TH1I("hVertexRank", "; Vertex Rank; Num. of Vertices", 100, -1.2e6, 1.2e6);
   hist->SetOption("hist GRIDX GRIDY XY");

   o["hVertexRankLog"] = hist = new TH1I("hVertexRankLog", "; Vertex LogRank; Num. of Vertices", 60, -30, 30);
   hist->SetOption("hist GRIDX GRIDY XY");

   o["hVertexPositionX"] = hist = new TH1I("hVertexPositionX", "; X Position, cm; Num. of Vertices", 50, -1, 1);
   hist->SetOption("hist GRIDX GRIDY XY");

   o["hVertexPositionY"] = hist = new TH1I("hVertexPositionY", "; Y Position, cm; Num. of Vertices", 50, -0.4, 0.4);
   hist->SetOption("hist GRIDX GRIDY XY");

   o["hVertexPositionZ"] = hist = new TH1I("hVertexPositionZ", "; Z Position, cm; Num. of Vertices", 50, -200, 200);
   hist->SetOption("hist GRIDX GRIDY XY");
}


/** */
void VertexHContainer::Fill(ProtoEvent &ev)
{
   VecBosEvent& event = (VecBosEvent&) ev;

   VecBosVertexPtrSetIter iVertex = event.mVertices.begin();

   for ( ; iVertex!=event.mVertices.end(); ++iVertex)
   {
      Fill(**iVertex);
   }
}


/** */
void VertexHContainer::Fill(VecBosVertex &vertex)
{
   ((TH1*) o["hVertexId"])->Fill(vertex.mId);
   ((TH1*) o["hVertexRank"])->Fill(vertex.mRank);
   ((TH1*) o["hVertexRankLog"])->Fill(vertex.mRankLog);
   ((TH1*) o["hVertexPositionX"])->Fill(vertex.mPosition.x());
   ((TH1*) o["hVertexPositionY"])->Fill(vertex.mPosition.y());
   ((TH1*) o["hVertexPositionZ"])->Fill(vertex.mPosition.z());
}


/** */
void VertexHContainer::FillDerived()
{
   Info("FillDerived()", "Called");
}


/** */
void VertexHContainer::PostFill()
{
   Info("PostFill", "Called");
}
