/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/


#ifndef Z0EventHContainer_h
#define Z0EventHContainer_h

#include "TDirectoryFile.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"


/**
 *
 */
class Z0EventHContainer : public PlotHelper
{
private:

   //TH1* fhPseudoMass_ch[N_SILICON_CHANNELS];


public:

   Z0EventHContainer();
   Z0EventHContainer(TDirectory *dir);

   void Fill(ProtoEvent &ev);

private:

   void BookHists();

   ClassDef(Z0EventHContainer, 1)
};

#endif
