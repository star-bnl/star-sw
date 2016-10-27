/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/


#ifndef EventDisplayHists_h
#define EventDisplayHists_h

#include "TDirectoryFile.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"

#include "VecBosEvent.h"


/**
 *
 */
class EventDisplayHists : public PlotHelper
{
public:

   EventDisplayHists();
   EventDisplayHists(TDirectory *dir);
   EventDisplayHists(TDirectory *dir, const VecBosEvent &ev);
   ~EventDisplayHists();

   void Fill(ProtoEvent &ev);
   //void FillDerived();
   //void FillDerived(PlotHelper &oc);
   //void PostFill();
   //void PostFill(PlotHelper &oc);

private:

   //TH1* fhPseudoMass_ch[N_SILICON_CHANNELS];

   void BookHists();
   void BookHists(const VecBosEvent &ev);


   ClassDef(EventDisplayHists, 1)
};

#endif
