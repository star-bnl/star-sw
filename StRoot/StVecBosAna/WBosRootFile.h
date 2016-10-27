#ifndef WBosRootFile_h
#define WBosRootFile_h

#include <map>
#include <set>
#include <string>

#include "VecBosRootFile.h"

#include "utils/PlotHelper.h"


/**
 * With this specialization of the ROOT's TFile we control the structure of the output file. In this
 * class we choose what should be saved (or not) and under what conditions. The decision is made on
 * event-by-event basis by calling the Fill(ProtoEvent &ev) function.
 */
class WBosRootFile : public VecBosRootFile
{

public:

   WBosRootFile();
   WBosRootFile(const char* fname, Option_t* option = "", Int_t isMc= 0, Bool_t isZ=kFALSE, const char* ftitle = "", Int_t compress = 1);
   ~WBosRootFile();

   void Fill(ProtoEvent &ev);
   void Fill(ProtoEvent &ev, ECut cut);

private:

   void BookHists();
};

#endif
