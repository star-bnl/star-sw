/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/


#ifndef KinemaHContainer_h
#define KinemaHContainer_h

#include "TDirectoryFile.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"


/**
 *
 */
class KinemaHContainer : public PlotHelper
{
private:

   //TH1* fhPseudoMass_ch[N_SILICON_CHANNELS];


public:

   KinemaHContainer();
   KinemaHContainer(TDirectory *dir);
   ~KinemaHContainer();

   using PlotHelper::FillDerived;
   using PlotHelper::PostFill;

   void Fill(ProtoEvent &ev);
   void FillDerived();
   void PostFill();

private:

   void BookHists();


   ClassDef(KinemaHContainer, 1)
};

#endif
