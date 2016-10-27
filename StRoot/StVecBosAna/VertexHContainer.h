/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/


#ifndef VertexHContainer_h
#define VertexHContainer_h

#include "TDirectoryFile.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"

#include "VecBosVertex.h"


/**
 *
 */
class VertexHContainer : public PlotHelper
{
private:

   //TH1* fhPseudoMass_ch[N_SILICON_CHANNELS];


public:

   VertexHContainer();
   VertexHContainer(TDirectory *dir);
   ~VertexHContainer();

   using PlotHelper::FillDerived;
   using PlotHelper::PostFill;

   void Fill(ProtoEvent &ev);
   void Fill(VecBosVertex &vertex);
   void FillDerived();
   void PostFill();

private:

   void BookHists();


   ClassDef(VertexHContainer, 1)
};

#endif
