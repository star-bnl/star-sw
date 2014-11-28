// $Id: create_ftpc_QA_refhists.C,v 1.2 2006/04/11 09:32:49 jcs Exp $
// $Log: create_ftpc_QA_refhists.C,v $
// Revision 1.2  2006/04/11 09:32:49  jcs
// improve comments
//
// Revision 1.1  2006/04/11 09:30:17  jcs
// sample macro to create one *.gif file for each FTPC Offline QA reference histogram
//
//
//------------------------------------------------------------------------
//  
// This macro produces one *.gif file for each Y2006 FTPC QA Reference Histogram
// The *.gif files are used in the "STAR Offline FTPC QA Reference Histograms  - Run 6" description:
//  /afs/rhic.bnl.gov/star/doc_public/www/ftpc/DataQualityControl/FTPC_QA_histograms_Y2006.html
// 
// For Year 2006, the EventQA histograms are divided into a set of general
// histograms and one or more multiplicity/trigger class histograms.
//
// Checking FTPC QA histograms with less than 400 pp events does not make much
// sense because of inconclusive statistics. For Y2006, the "high tower histogram"
// set is used because it contains more events than the minbias set
//
// ATTENTION!!!!!!!: The *.gif files ONLY contain the correct line attributes 
//                   when this macro is run interactively
//
//------------------------------------------------------------------------

int sizeOfCharPtr = sizeof(Char_t*);

class StChain;
StChain *chain;

class StIOMaker;
StIOMaker *IOMk=0;

//------------------------------------------------------------------------

void create_ftpc_QA_refhists(
  const Char_t *MainFile=
     "/star/xtp/jcs/st_physics_7075188_raw_1030004.hist.root"
)
{             

  cout << "create_ftpc_refhists.C, input hist file = " 
       << MainFile << endl;

//
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");

//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();

// setup chain with IOMaker - can read in .dst.root, .dst.xdf files
  StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
  IOMk->SetDebug();
  IOMk->SetIOMode("r");
  IOMk->SetBranch("*",0,"0");                 //deactivate all branches
  IOMk->SetBranch("histBranch",0,"r"); //activate hist Branch

// Set the default canvas style to plain (so it won't print out grey!)
    gROOT->SetStyle("Plain");
    TCanvas *canvas = new TCanvas("CanvasName"," STAR Maker Histogram Canvas",1);
    gStyle->SetOptStat(111111);
    gStyle->SetStatStyle(0);
    gStyle->SetOptDate(0);
    canvas->SetGridx(0);
    canvas->SetGridy(0); 

// --- now execute chain member functions
  chain->Init();

  TDataSet *ds=0;
  TDataSet *obj=0;
  int i=0;
  int istat=0;
  int countev=0;
  int countevhds=0;

  chain->Clear();
  istat = chain->Make(i);

//  Ftpc Reference Histogram list for Y2006
  cout<<"sizeOfCharPtr = "<<sizeOfCharPtr<<endl;
  Char_t* sdefList[] = {
    "StEQaEvsumTotChgF",
    "fcl_chargestepW",
    "fcl_chargestepE",
    "StEHTQaPointFtpc",
    "StEHTQaPointXYFtpcE",
    "StEHTQaPointXYFtpcW",
    "StEHTQaPointPadTimeFtpcW",
    "StEHTQaPointPadTimeFtpcE",
    "StEHTQaPointPlaneF",
    "StEHTQaGtrkXfYfFE",
    "StEHTQaGtrkXfYfFW",
    "StEHTQaGtrkPsiF",
    "StEHTQaGtrkPtF",
    "StEHTQaGtrkEtaF",
    "StEHTQaGtrkPF",
    "StEHTQaGtrkNPntF",
    "StEHTQaGtrkGoodF",
    "StEHTQaGtrkImpactrF",
    "StEHTQaPtrkMeanPtF",
     "StEHTQaPtrkMeanEtaF",
    "StEHTQaPtrkEtaF",
    "StEHTQaPtrkPtF",
    "StEHTQaVtxFtpcETpcXY",
    "StEHTQaVtxFtpcETpcZ",
    "StEHTQaVtxFtpcWTpcZ",
    "StEHTQaVtxFtpcWTpcXY"
  };  
    Int_t lengofList = sizeof(sdefList)/sizeOfCharPtr;
cout<<"sdefList[0] = "<<sdefList[0]<<endl;
    cout<<"sizeof(sdefList) = "<<sizeof(sdefList)<<" sizeOfCharPtr = "<<sizeOfCharPtr<<" lengofList = "<<lengofList<<endl;

// Now look at the data in the event:
    int countObj=0;
    int countHist=0;

    if (!istat) {

    countev++;

    cout << " start event # " << countev << endl;

      ds=chain->GetDataSet("hist");
      TDataSetIter tabiter(ds);
      if (ds) {

        countevhds++;

        TDataSetIter nextHistList(ds);
        St_ObjectSet *histContainer = 0;
        TList *dirList = 0;

// loop over directories:
        while (histContainer = (St_ObjectSet *)nextHistList()) {
          dirList = (TList *) histContainer->GetObject();

          cout << " QAInfo: found directory: " <<
                    histContainer->GetName() << endl;

          countObj++;
  
          if (strcmp((Char_t *)histContainer->GetName(),"EventQAHist") == 0) {
             cout << "QAInfo: Process histograms in directory EventQAHist" <<endl;

// Notes for future reference (if we want to generalize this...)
//    dirList is returned  0 for non-histogram file
//       in that case, use GetList instead of GetObject

         TIter nextHist(dirList);
         TObject  *o = 0;
         TH1* hobjradialW = NULL;
         TH1* hobjradialE = NULL;
// loop over histograms in the directory:
        while (o= nextHist()) {
//cout<<" o->GetName()) "<<o->GetName()<<endl;

           canvas->SetLogy(0);
           Int_t ilg = 0;
           Char_t filename[50];
           for (ilg=0;ilg<lengofList;ilg++) {
              if (strcmp(sdefList[ilg],o->GetName()) == 0) {
                  countHist++;
                  cout << " QAInfo:   Hist name: " << o->GetName() <<
                     " ==> Title: " << o->GetTitle() << endl;
                  if ( (strcmp("fcl_chargestepW",o->GetName()) == 0) 
                    || (strcmp("fcl_chargestepE",o->GetName()) == 0)
                    || (strcmp("StEHTQaGtrkEtaF",o->GetName()) == 0)
                    || (strcmp("StEHTQaGtrkPtF",o->GetName()) == 0)
                    ||(strcmp("StEHTQaGtrkPF",o->GetName()) == 0)
                  ) canvas->SetLogy(1);
                 //  Now draw the actual histogram to canvas and to gif file
                 if ( (strcmp("StEHTQaPointXYFtpcE",o->GetName()) == 0)
                   || (strcmp("StEHTQaPointXYFtpcW",o->GetName()) == 0)
                   || (strcmp("StEHTQaPointPadTimeFtpcE",o->GetName()) == 0)
                   || (strcmp("StEHTQaPointPadTimeFtpcW",o->GetName()) == 0)
                   || (strcmp("StEHTQaGtrkGoodF",o->GetName()) == 0)
                   || (strcmp("StEHTQaGtrkXfYfFW",o->GetName()) == 0)
                   || (strcmp("StEHTQaGtrkXfYfFE",o->GetName()) == 0)
                   || (strcmp("StEHTQaVtxFtpcETpcXY",o->GetName()) == 0)
                   || (strcmp("StEHTQaVtxFtpcWTpcXY",o->GetName()) == 0)) {
                    o->Draw("Box");
                    if (strcmp("StEHTQaGtrkGoodF",o->GetName()) == 0 
                         && o->InheritsFrom("TH1")) {
                       TH1* hobj = (TH1*) o;
                    // Limit both x & y ranges together
                       Float_t mean1 = hobj->GetMean(1);
                       Float_t mean2 = hobj->GetMean(2);
                       Float_t window1 = hobj->GetRMS(1);
                       Float_t window2 = hobj->GetRMS(2);
                       Float_t bwid = hobj->GetBinWidth(1);
                       if (window1 < bwid) window1  = bwid;
                       if (window2 < bwid) window2  = bwid;
                       Float_t lo = TMath::Min(mean1-5*window1,mean2-5*window2);
                       Float_t hi = TMath::Max(mean1+5*window1,mean2+5*window2);
                       hobj->SetAxisRange(lo,hi,"X");
                       hobj->SetAxisRange(lo,hi,"Y");
                       TLine ruler;ruler.SetLineColor(46);
                       ruler.DrawLineNDC(0.1,0.1,0.9,0.9);
                    } 
                 }
                 else {
                    if (strcmp("StEHTQaPointFtpc",o->GetName()) == 0 
                         && o->InheritsFrom("TH1")) {
                       TH1* hobj = (TH1*) o;
                       Float_t mean = hobj->GetMean(1);
                       Float_t window = hobj->GetRMS(1);
                       Float_t bwid = hobj->GetBinWidth(1);
                       if (window < bwid) window  = bwid;
                       hobj->SetAxisRange(mean-5*window,mean+5*window,"X");
                     } // StEHTQaPointFtpc 
                    o->Draw();
                 }
                 sprintf(filename,"%s.gif",o->GetName());
                 canvas->Print(filename);
              }
           }  // sdefList
           if ((strcmp("fcl_radialW",o->GetName()) == 0)) hobjradialW = (TH1*) o;
           if ((strcmp("fcl_radialE",o->GetName()) == 0)) hobjradialE = (TH1*) o;
//cout<<"hobjradialW = "<<hobjradialW<<" hobjradialE = "<<hobjradialE<<endl;
           if (hobjradialW && hobjradialE) {
              hobjradialW->SetStats(kFALSE);
              hobjradialW->GetXaxis()->SetRangeUser(7.0,9.0);
              hobjradialE->SetStats(kFALSE);
              hobjradialE->GetXaxis()->SetRangeUser(7.0,9.0);
              if ( hobjradialW->GetMaximum() >= hobjradialE->GetMaximum()) {    
                 //hobjradialW->SetTitle((TString)"FTPCW+E cluster radial position");
                 hobjradialW->SetTitle("FTPCW+E cluster radial position");
                 hobjradialE->SetTitle(hobjradialW->GetTitle());
                 hobjradialW->Draw();
                 hobjradialE->Draw("Same");
                 canvas->Modified();
                 TLine ruler; ruler.SetLineColor(kBlack);
                 ruler.SetLineWidth(2);
                 ruler.DrawLine(7.8,0.,7.8,hobjradialW->GetMaximum());
                 sprintf(filename,"%s.gif",hobjradialW->GetName());
               } else {
                 hobjradialE->SetTitle((TString)"FTPCE+W cluster radial position");
                 hobjradialW->SetTitle(hobjradialE->GetTitle());
                 hobjradialE->Draw();
                 canvas->Modified();
                 hobjradialW->Draw("Same");
                 TLine ruler; ruler.SetLineColor(kBlack);
                 ruler.SetLineWidth(2);
                 ruler.DrawLine(7.8,0.,7.8,hobjradialE->GetMaximum());
                 sprintf(filename,"%s.gif",hobjradialE->GetName());
               }  

                // make a legend
               TLegend *legend = new TLegend(0.75,0.85,0.98,0.95);
               legend->SetFillColor(0);
               legend->SetHeader("Legend");
               legend->SetMargin(0.25);
               legend->AddEntry(hobjradialE,"FtpcEast","l");
               legend->AddEntry(hobjradialW,"FtpcWest","l");
               legend->Draw();
               canvas->Print(filename);
               hobjradialW = NULL;
               hobjradialE = NULL;
            }  // hobjradialW && hobjradialE
          } // nextHist
         } // histContainer == "EventQA"
        } // histContainer
      } // ds

    cout << " QAInfo: event # " << countev
            << ", # directories found = " << countObj
            << ", # hist found = " << countHist
            << endl << endl;

    } // istat

    else   // if (istat)
      {
      cout << "Last event processed. Status = " << istat << endl;
    }

   cout <<" create_ftpc_refhists.C, end of macro" << endl;

}


