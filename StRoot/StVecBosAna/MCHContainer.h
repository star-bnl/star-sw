/*****************************************************************************
 *                                                                           *
 *                                                                           *
 *****************************************************************************/


#ifndef MCHContainer_h
#define MCHContainer_h

#include "TDirectoryFile.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"

#include "VecBosTrack.h"


/**
 *
 */
class MCHContainer : public PlotHelper
{
private:

   //TH1* fhPseudoMass_ch[N_SILICON_CHANNELS];


public:

   MCHContainer();
   MCHContainer(TDirectory *dir, Int_t);

   void Fill(ProtoEvent &ev);
   void PostFill();
   void PostFill(PlotHelper &oc) {}

private:

   void BookHists();
   Int_t mMcType;
   Double_t Wy_gen, Wy_rec, An_evol;
   //static const int PtBins = 7;
   static const int PtBins = 6;
   Double_t xBinsPt[PtBins+1];

   static const int RapBins = 3;
   Double_t xBinsRap[RapBins+1];
   Double_t xBinsRapPR[RapBins+1];
   Double_t xBinsRapMR[RapBins+1];

   ClassDef(MCHContainer, 1)
};

#endif
