#ifndef VecBosRootFile_h
#define VecBosRootFile_h

#include <map>
#include <set>
#include <string>

#include "TFile.h"

#include "utils/PlotHelper.h"


enum ECut {kCUT_UNKNOWN,  kCUT_EVENT_NOCUT,
           kCUT_VERTICES, kCUT_VERTICES_GOOD,
           kCUT_TRACKS,   kCUT_TRACKS_GOOD,
           kCUT_BARREL, kCUT_ENDCAP, kCUT_CUT,
           kCUT_EVENT_HAS_CANDIDATE_TRACK,
           kCUT_EVENT_HAS_CANDIDATE_TRACK_PT15,
           kCUT_EVENT_W,
           kCUT_EVENT_W_PLUS,
           kCUT_EVENT_W_MINUS,
           kCUT_EVENT_Z,
           kCUT_EVENT_PASS_WBOS,
           kCUT_POSITIVE_EVENT_PASS_WBOS,
           kCUT_NEGATIVE_EVENT_PASS_WBOS,
           kCUT_EVENT_PASS_WBOS_PT15,
           kCUT_POSITIVE_EVENT_PASS_WBOS_PT15,
           kCUT_NEGATIVE_EVENT_PASS_WBOS_PT15,
           kCUT_EVENT_PASS_Z0_ELECTRONPT,
           kCUT_EVENT_PASS_Z0_FINAL,
           kCUT_EVENT_PASS_QCD,
           kCUT_EVENT_PASS_QCD_PT15,
           kCUT_POSITIVE_EVENT_PASS_QCD,
           kCUT_NEGATIVE_EVENT_PASS_QCD,
           kCUT_POSITIVE_EVENT_PASS_QCD_PT15,
           kCUT_NEGATIVE_EVENT_PASS_QCD_PT15,
           kCUT_EVENT_HAS_JETRECOIL,
           kCUT_EVENT_HAS_TRACKRECOIL,
           kCUT_ELERACK};

typedef std::map<ECut, std::set<PlotHelper*> >   Cut2PlotHelperMap;
typedef Cut2PlotHelperMap::iterator              Cut2PlotHelperMapIter;


/**
 * With this specialization of the ROOT's TFile we control the structure of the output file. In this
 * class we choose what should be saved (or not) and under what conditions. The decision is made on
 * event-by-event basis by calling the Fill(ProtoEvent &ev) function.
 */
class VecBosRootFile : public TFile
{
public:

   VecBosRootFile();
   VecBosRootFile(const char* fname, Option_t* option = "", Int_t isMc = 0, Bool_t isZ=kFALSE, const char* ftitle = "", Int_t compress = 1);

   using TObject::SaveAs;

   PlotHelper* GetHists();
   void SetHists(PlotHelper &hists);
   void SetIsMc(Int_t isMc) { fIsMc = isMc; }
   void SetIsZ(Bool_t isZ) { fIsZ = isZ; }
   virtual void Fill(ProtoEvent &ev);
   virtual void Fill(ProtoEvent &ev, ECut cut);
   virtual void FillDerived();
   virtual void PostFill();
   void SaveAs(std::string pattern="^.*$", std::string dir=".");
	void UpdMinMaxFill(UInt_t fillId);
	void UpdMinMaxTime(time_t time);
   void Print(const Option_t* opt="") const;
   void PrintAsPhp(FILE *f=stdout) const;
   void Close(Option_t* option = "");

protected:

   PlotHelper        *fHists;
   Cut2PlotHelperMap  fHistCuts;
   Float_t            fMinFill;
   Float_t            fMaxFill;
   time_t             fMinTime;
   time_t             fMaxTime;
   Int_t              fIsMc;
   Bool_t             fIsZ;

private:

   void BookHists();
};

#endif
