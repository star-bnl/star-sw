/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/


#ifndef EventHContainer_h
#define EventHContainer_h

#include "TDirectoryFile.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"


/**
 *
 */
class EventHContainer : public PlotHelper
{
private:

   //TH1* fhPseudoMass_ch[N_SILICON_CHANNELS];


public:

   EventHContainer();
   EventHContainer(TDirectory *dir);

   void Fill(ProtoEvent &ev);

private:

   void BookHists();

   ClassDef(EventHContainer, 1)
};

#endif
