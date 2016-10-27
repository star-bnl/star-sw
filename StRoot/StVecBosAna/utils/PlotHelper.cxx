#include "PlotHelper.h"

#include "TGraph.h"
#include "TStyle.h"
#include "TLatex.h"


ClassImp(PlotHelper)

using namespace std;


/** Default constructor. */
PlotHelper::PlotHelper() : TObject(), fSignature(""), fDir(), o(),
   d()
{
   fDir = gDirectory;
   fDir->cd();
}


/** */
PlotHelper::PlotHelper(TDirectory *dir) : TObject(),
   fSignature(""), fDir(dir), o(), d()
{
}


/** */
std::string PlotHelper::GetSignature() const
{
   return fSignature;
}


/** */
void PlotHelper::SetSignature(std::string signature)
{
   fSignature = signature;
}


/** */
TDirectory * PlotHelper::GetDir()
{
   return fDir;
}


/** */
void PlotHelper::SetDir(TDirectory *dir)
{
   fDir = dir;
   fDir->cd();
}


/** */
void PlotHelper::ReadFromDir()
{
   ObjMapIter io;
   
   for (io=o.begin(); io!=o.end(); ++io) {
      //cout << "Considering dir: " << fDir->GetName() << endl;
      //fDir->ls();
      //fDir->Name();

      // first find the key
      //fDir->cd();
      TKey *key = fDir->GetKey(io->first.c_str());
      //TKey *key = gDirectory->GetKey(io->first.c_str());

      if (!key) {
         Warning("ReadFromDir", "Key %s not found", io->first.c_str());
         continue;
      }

      //cout << "Found key: " << endl;
      //key->Print();

      // read obj into memory and assign a pointer
      TObject* tmpObj = 0;

      if (io->second) {
         //delete io->second;
         tmpObj = io->second; // save pointer to the original histogram/object
      }

      // Assign a new pointer to the object read from dir
      io->second = key->ReadObj();

      // overwrite options
      if (((TClass*) io->second->IsA())->InheritsFrom("TH1") && tmpObj) {
         ((TH1*) io->second)->SetOption( ((TH1*) tmpObj)->GetOption() );
      }

      // delete the original object
      delete tmpObj;
   }

   // loop over sub containers and call this function recursively
   PlotHelperMapIter isub;
   
   for (isub=d.begin(); isub!=d.end(); ++isub) {

      TKey *key = fDir->GetKey(isub->first.c_str());
      
      if (!key) {
         Warning("ReadFromDir", "Directory key %s not found", isub->first.c_str());
         continue;
      }

      //cout << "Found key: ";
      //key->Print();

      //key->ls();
      //fDir->cd(isub->first.c_str());

      //gDirectory->ls();

      TDirectory *subdir = (TDirectory*) key->ReadObj();
      //TDirectory *subdir = fDir->GetDirectory(isub->first.c_str());
      isub->second->fDir = subdir;

      //if (!subdir) {
      //   //subdir->Print();
      //   //subdir->ls();
      //} else {
      //   cout << "ERROR: subdir not found: " << isub->first << endl;
      //}

      // replace the object with the one from this dir/file
      //isub->second = tmpContainer;
      //isub->second.fDir = gDirectory;
      isub->second->ReadFromDir();
   }
}


/** */
void PlotHelper::ReadFromDir(TDirectory *dir)
{
   fDir = dir;
   fDir->cd();
   ReadFromDir();
}


/**
 * Recursively adds objects (histograms) from another PlotHelper to this
 * one.
 */
void PlotHelper::Add(PlotHelper *oc)
{ //{{{
   o.insert(oc->o.begin(), oc->o.end());

   PlotHelperMapIter isubd;
   pair<PlotHelperMapIter, bool> result;

   //oc->Print();

   for (isubd=oc->d.begin(); isubd!=oc->d.end(); ++isubd) {

      //isubd->second->Print();
         
      result = d.insert(*isubd);
      if (result.second) {
         //d
         continue;
      }

      // This and oc objects contain subdir with the same name. Therefore add
      // objects to the existing one. result.first is a pointer to pair<string, PlotHelper*>
      PlotHelper *existingOc = (result.first)->second;
      existingOc->Add(isubd->second);
   }
} //}}}


/** Default destructor. */
PlotHelper::~PlotHelper()
{
   //if (!fDir) {
   //   Fatal("Delete(Option_t* option)", "fDir not defined\n");
   //}
   //fDir->cd();

   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      //isubd->second->Delete(option);
      delete isubd->second;
      //if (fDir && !((TClass*) fDir->IsA())->InheritsFrom("TFile")) { delete fDir; fDir = 0; }
   }

   ObjMapIter io;

   for (io=o.begin(); io!=o.end(); ++io) {
      //sprintf(cName, "c_%s", io->first.c_str());
      //cout << "Deleting: " << io->first << endl;
      if (io->second) {
         io->second->Delete();
         //delete io->second;
      }
   }

   //fDir->Close();
   fDir->Delete("*;*");
   fDir->GetListOfKeys()->Delete();
   //if (fDir && !((TClass*) fDir->IsA())->InheritsFrom("TFile")) { delete fDir; fDir = 0; }
}


/** */
void PlotHelper::Print(const Option_t* opt) const
{
   //opt = "";

   //printf("PlotHelper:\n");

   if (!fDir) {
      Error("Print", "fDir not defined");
      return;
   }

   fDir->cd();

   ObjMapConstIter io;

   for (io=o.begin(); io!=o.end(); ++io) {
      if (io->second) io->second->Print();
   }

   PlotHelperMapConstIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      cout << "Content of " << isubd->first << endl;
      isubd->second->Print();
   }
}


/** */
void PlotHelper::SaveAllAs(TCanvas &canvas, std::string pattern, string path, Bool_t thumbs)
{
   if (gSystem->mkdir(path.c_str()) < 0)
      Warning("SaveAllAs", "Perhaps dir already exists: %s", path.c_str());
   else {
      gSystem->Chmod(path.c_str(), 0775);
      Info("SaveAllAs", "Created dir: %s", path.c_str());
   }

   ObjMapIter io;

   for (io=o.begin(); io!=o.end(); ++io)
   {
      // For shorthand
      string   sObjName = io->first;
      TObject *obj      = io->second;

      if (!obj) {
         Error("SaveAllAs", "No object found for key %s. Skipping...", io->first.c_str());
         continue;
      }

      string sCanvasName = "c_" + sObjName;

      if (thumbs) sCanvasName += "_thumb";

      canvas.cd();
      canvas.SetName(sCanvasName.c_str());
      canvas.SetTitle(sCanvasName.c_str());

      if ( ((TClass*) obj->IsA())->InheritsFrom("THStack") )
      {
         obj->Draw("nostack");
      }
      else if ( ((TClass*) obj->IsA())->InheritsFrom("TH1") )
      {
         TH1  *hobj     = (TH1*) obj;
			char *hoptions = (char*) hobj->GetOption();

         char *l = strstr( hoptions, "NOIMG");
         if (l) continue;

         l = strstr( hoptions, "LOGZ");
         //printf("XXX1: set logz %s\n", hobj->GetOption());
         if (l) {
            memset(l, ' ', 4);
            canvas.SetLogz(kTRUE);
         } else canvas.SetLogz(kFALSE);

         l = strstr( hoptions, "XX");
         if (l) {
            memset(l, ' ', 2);
            canvas.SetLogx(kTRUE);
         } else canvas.SetLogx(kFALSE);

         l = strstr( hoptions, "XY");
         if (l) {
            memset(l, ' ', 2);
            canvas.SetLogy(kTRUE);
         } else canvas.SetLogy(kFALSE);

         l = strstr( hoptions, "GRIDX");
         if (l) {
            memset(l, ' ', 5);
            canvas.SetGridx(kTRUE);
         } else canvas.SetGridx(kFALSE);

         l = strstr( hoptions, "GRIDY");
         if (l) {
            memset(l, ' ', 5);
            canvas.SetGridy(kTRUE);
         } else canvas.SetGridy(kFALSE);

         l = strstr( hoptions, "DUMMY");
         if (l) {
            memset(l, ' ', 5);
            hobj->SetStats(kFALSE);
         }

         obj->Draw( hoptions );

         canvas.Update();

         // Position stat box of the main histogram object
         TPaveStats *stats = (TPaveStats*) (hobj)->FindObject("stats");

         // There is no way to set these parameters in gStyle so, do it here
         if (stats) {
            //stats->SetShadowColor(0);
            stats->SetLineWidth(1);

            stats->SetX1NDC(0.80);
            stats->SetX2NDC(0.99);
            stats->SetY1NDC(0.60);
            stats->SetY2NDC(0.90);
         }

         //printf("could not find stats in %s\n", io->first.c_str());

         // Now check if there are other associated objects like functions and graphs
         TList* list = hobj->GetListOfFunctions();

         //cout << "XXX print" << endl;
         //if (list) list->Print();

         TIter  next(list);
         UShort_t iStat = 0;

         while ( TObject *iObj = (TObject*) next() )
         {
            if ( !iObj ) continue;

            if ( ( (TClass*) iObj->IsA() )->InheritsFrom("TLine") ) {
               iObj->Draw();
               continue;
            }

            // Consider only TGraph objects
            if ( ! (( (TClass*) iObj->IsA() )->InheritsFrom("TGraph")) ) continue;

            TGraph *grObj = (TGraph*) iObj;

            if ( ((TGraph*) grObj)->GetN() <= 0) continue;

            //grObj->Print();

            TPaveStats *stats = (TPaveStats*) ((TGraph*) grObj)->FindObject("stats");

            if (stats) {
               //cout << "stats found:" << endl;
               //stats->Print();
               //stats->SetOptStat(0);
               //stats->SetOptFit(1111);

               stats->SetLineColor(grObj->GetMarkerColor());
               //stats->SetShadowColor(0);
               stats->SetLineWidth(2);

               stats->SetX1NDC(0.80);
               stats->SetX2NDC(0.99);
               stats->SetY1NDC(0.42 - iStat*0.18);
               stats->SetY2NDC(0.60 - iStat*0.18);

               iStat++;
            } else {
               //cout << "stats not found" << endl;
            }
         }
      } else { // just draw the object
         obj->Draw();
      }

      TLatex signature(0, 0, fSignature.c_str());
      signature.SetTextSize(0.035);
      UInt_t w, h;
      signature.GetTextExtent(w, h, signature.GetTitle());
      //signature.DrawTextNDC(0.98-(w/(Float_t) canvas.GetWw()) - gStyle->GetPadRightMargin(), 1-(h/(Float_t) canvas.GetWh()), signature.GetTitle());
      signature.DrawTextNDC(0.98-(w/(Float_t) canvas.GetWw()), 1-(h/(Float_t) canvas.GetWh()), signature.GetTitle());

      canvas.RedrawAxis("g");

      //if (obj) obj->Print();

      //string sFileName = path + "/" + sCanvasName + ".png";
      string sFileName = path + "/" + sCanvasName + ".eps";

      if (TPRegexp(pattern).MatchB(sFileName.c_str())) {

         // XXX Print out for debugging
         //obj->Print("all");
         //hobj->GetListOfFunctions()->Print("all");

         canvas.SaveAs(sFileName.c_str());
         gSystem->Chmod(sFileName.c_str(), 0775);
      } else {
         //Info("SaveAllAs", "Histogram %s name does not match pattern. Skipped", sFileName.c_str());
      }
   }

   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {

      //XXX Temporary conditions for test only
      //if (isubd->first.find("channel28") == string::npos) continue;
      //if (isubd->first.find("preproc") == string::npos) continue;

      // Overwrite signature by setting it to the parent one
      isubd->second->SetSignature(fSignature);

      string parentPath = path;
      path += "/" + isubd->first;
      isubd->second->SaveAllAs(canvas, pattern, path, thumbs);
      path = parentPath;
   }
}


/** */
void PlotHelper::SaveHStackAs(TCanvas &canvas, THStack &hstack, std::string path)
{ //{{{
   // Require at least two elements in the hstack
   if (hstack.GetHists()->GetSize() < 2) {
      Error("SaveHStackAs", "At least 2 histograms required in the HStack. Skipping saving request for %s", hstack.GetName());
      return;
   }

   canvas.cd();


   TList    *hists  = hstack.GetHists();
   TIter     next(hists);
   UShort_t  iNHist = 0;
   UShort_t  iStat  = 0;

   while ( TH1* iHist = (TH1*) next() )
   {
      if ( !iHist ) continue;

      if (iNHist == 0)
      {
			char *hoptions = (char*) iHist->GetOption();
         char *l = strstr(hoptions, "LOGZ");

         if (l) {
            memset(l, ' ', 4);
            canvas.SetLogz(kTRUE);
         } else { canvas.SetLogz(kFALSE); }

         iHist->Draw();
      } else {
         iHist->Draw("sames");
      }

      canvas.Update();

      // Position stat boxes
      TPaveStats *stats = (TPaveStats*) iHist->FindObject("stats");

      if (stats) {
         //cout << "stats found:" << endl;
         //stats->Print();
         //stats->SetOptStat(0);
         //stats->SetOptFit(1111);

         //stats->SetLineColor(grObj->GetMarkerColor());
         //stats->SetShadowColor(0);
         stats->SetLineWidth(1);

         stats->SetX1NDC(0.80);
         stats->SetX2NDC(0.99);
         stats->SetY1NDC(0.60 - iStat*0.30);
         stats->SetY2NDC(0.90 - iStat*0.30);

         iStat++;
      } else {
         //cout << "stats not found" << endl;
      }

      iNHist++;
   }

   //hstack.Draw("nostack");

   canvas.SetName(hstack.GetName());
   canvas.SetTitle(hstack.GetName());

   TLatex signature(0, 0, fSignature.c_str());
   signature.SetTextSize(0.035);
   UInt_t w, h;
   signature.GetTextExtent(w, h, signature.GetTitle());
   //signature.DrawTextNDC(0.98-(w/(Float_t) canvas.GetWw()/2) - gStyle->GetPadRightMargin(), 1-(h/(Float_t) canvas.GetWh()), signature.GetTitle());
   signature.DrawTextNDC(0.98-(w/(Float_t) canvas.GetWw()), 1-(h/(Float_t) canvas.GetWh()), signature.GetTitle());

   //string sFileName = path + "/" + canvas.GetName() + ".png";
   string sFileName = path + "/" + canvas.GetName() + ".eps";

   //if (TPRegexp(pattern).MatchB(sFileName.c_str()))
   //{
      canvas.SaveAs(sFileName.c_str());
      gSystem->Chmod(sFileName.c_str(), 0775);
   //} else {
   //   //Info("SaveAllAs", "Histogram %s name does not match pattern. Skipped", sFileName.c_str());
   //}
} //}}}


/** */
void PlotHelper::Draw(TCanvas &c)
{ //{{{
   //Bool_t isBatch = gROOT->IsBatch();

   //gROOT->SetBatch(kTRUE);

   ObjMapIter io;

   char cName[256];

   for (io=o.begin(); io!=o.end(); ++io) {

      sprintf(cName, "c_%s", io->first.c_str());
      c.cd();
      c.SetName(cName);
      c.SetTitle(cName);

      //if (io->second) (io->second)->Draw();
      if (io->second) io->second->Print();
   }

   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->Draw(c);
   }

   //gROOT->SetBatch(isBatch);
} //}}}


/** */
Int_t PlotHelper::Write(const char* name, Int_t option, Int_t bufsize)
{
   if (!fDir) {
      Fatal("Write", "Directory fDir not defined");
   }

   fDir->cd();
   //fDir->Write();

   ObjMapIter io;

   for (io=o.begin(); io!=o.end(); ++io) {
      //sprintf(cName, "c_%s", io->first.c_str());
      if (io->second) io->second->Write();
   }

   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      cout << "Writing content of " << isubd->first << endl;
      isubd->second->Write();
   }

   //fDir->Write();

   return 0;//((TObject*)this)->Write(name, option, bufsize);
}


/** */
Int_t PlotHelper::Write(const char* name, Int_t option, Int_t bufsize) const
{
   return ((const PlotHelper*) this)->Write(name, option, bufsize);
}


/** */
void PlotHelper::PreFillPassOne() {}


/** */
void PlotHelper::FillDerivedPassOne()
{ //{{{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->FillDerivedPassOne();
   }
} //}}}


/** */
void PlotHelper::PostFillPassOne(PlotHelper *oc)
{ //{{{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->PostFillPassOne(oc);
   }
} //}}}


/** */
void PlotHelper::PreFill()
{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->PreFill();
   }
}


/** */
void PlotHelper::Fill(ProtoEvent &ev)
{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->Fill(ev);
   }
}


/** */
void PlotHelper::FillDerived()
{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->FillDerived();
   }
}


/** */
void PlotHelper::FillDerived(PlotHelper &ph)
{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->FillDerived(ph);
   }
}


/** */
void PlotHelper::PostFill()
{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->PostFill();
   }
}


/** */
void PlotHelper::PostFill(PlotHelper &ph)
{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->PostFill(ph);
   }
}


/** */
void PlotHelper::UpdateLimits()
{ //{{{
   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->UpdateLimits();
   }
} //}}}


/** */
void PlotHelper::Delete(Option_t* option)
{ //{{{
   if (!fDir) {
      Fatal("Delete(Option_t* option)", "fDir not defined\n");
   }

   fDir->cd();

   ObjMapIter io;

   for (io=o.begin(); io!=o.end(); ++io) {
      //sprintf(cName, "c_%s", io->first.c_str());
      //cout << "YYY: " << io->first << endl;
      if (io->second) {
         io->second->Delete(option);
         //delete io->second;
      }
   }

   PlotHelperMapIter isubd;

   for (isubd=d.begin(); isubd!=d.end(); ++isubd) {
      isubd->second->Delete(option);
      //delete isubd->second;
   }

   //fDir->Close();
   fDir->Delete();
} //}}}
