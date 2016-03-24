#ifndef JetHContainer_h
#define JetHContainer_h

#include "TDirectoryFile.h"

#include "VecBosJet.h"

#include "utils/PlotHelper.h"
#include "utils/ProtoEvent.h"


/**
 *
 */
class JetHContainer : public PlotHelper
{
public:

   JetHContainer();
   JetHContainer(TDirectory *dir);

   void Fill(ProtoEvent &ev);
   void Fill(VecBosJet &vbJet);
   void Fill(VecBosJetPtrSet &vbJets);

private:

   void BookHists();

   ClassDef(JetHContainer, 1)
};

#endif
