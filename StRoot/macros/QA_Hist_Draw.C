// $Id: QA_Hist_Draw.C,v 1.7 1999/05/21 15:33:52 kathy Exp $
// $Log: QA_Hist_Draw.C,v $
// Revision 1.7  1999/05/21 15:33:52  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Kathy Turner
// what it does: read root histogram file and send all histograms to a
//                postscript file
//         !! Written by Valery, but Kathy will maintain
//=======================================================================
//
TCanvas *QACanvas = 0;
TBrowser *QABrowser = 0;
void QA_Hist_Draw(
     const Char_t *firstHistName="*",const Char_t *lastHistName="*",
     const Char_t *fileName="/diskA/star/kathy/output/psc0049_08_40evts_3EV.root",
     const Char_t *psFile="QA_hist.ps")
{ 
    cout << endl   
         << " Usage:  QA_Hist_Draw( " << endl
         << "                        const Char_t *firstHistName=\"" << firstHistName << "\","   << endl
         << "                        const Char_t *lastHistName=\""  << lastHistName  << "\""    << endl
         << "                        const Char_t *fileName =\""     << fileName      << "\","   << endl
         << "                        const Char_t *psFile=\""        << psFile        << "\","   << endl
         << "                      );" << endl 
         << " Note: firstHistName = \"*\" is by default and means ALL histograms from the file should be drawn" << endl
         << " ----- lastHistName  = \"*\" is by default and means ALL histograms by the end of file should be drawn" << endl
         << endl ;

  //  check file first
  Long_t id;
  Long_t size;
  Long_t flags; 
  Long_t modtime;
  Char_t *exFileName = gSystem->ExpandPathName(fileName);
  if (gSystem->GetPathInfo(exFileName, &id, &size, &flags, &modtime)) 
  {
   cerr << " *** Error ***  Can not find file: \"" << fileName << "\"" << endl;
   delete [] exFileName;
   exFileName=0;
   return;
  }
  Char_t *exPsFile =  gSystem->ExpandPathName(psFile);
//  TString tmodTime = ctime(modtime);
  TFile file1(exFileName);  
  // TFile file1("/diskA/star/kathy/output/psc0049_08_40evts_3EV.root");  
  // file1.ls();
  TList *keys = file1.GetListOfKeys();
  if (keys) {
    const Int_t xSize = 2;
    const Int_t ySize = 3;
    const Int_t numPads = xSize*ySize;

    Int_t width  = 20;
    Int_t height = 27;
    gStyle->SetOptStat(0);
    gStyle->SetPaperSize(width, height);
    QACanvas = new TCanvas("Banner","Canvas Title",30*height,30*width);
    TPostScript p5(exPsFile);
    TH1F dummy("QA","MDC2",1,0,1);
 // Create banner page   
//    QACanvas->Range(0,0,1,1);
    QACanvas->SetFillColor(19);
    QACanvas->SetBorderSize(2);

    p5->NewPage();
   
    dummy.SetMinimum(0);
//    dummy.Draw();
    gStyle->SetOptStat(111111);
    TString banner  =  "Input: \"";
    banner += exFileName;
    banner += "\"";
    TPaveLabel *pl = new TPaveLabel(0.06, 0.75, 0.9, 0.9, banner.Data(), "br");
    pl->SetFillColor(18);
    pl->SetTextSize(0.7);
    pl->Draw();
   
    banner  =  "Output: \"";
    banner += exPsFile;
    banner += "\"";
    pl = new TPaveLabel(0.06, 0.55, 0.9, 0.7, banner.Data(), "br");
    pl->SetFillColor(18);
    pl->SetTextSize(0.7);
    pl->Draw();
   
   banner  =  "Starting from: \"";
   if (strcmp(firstHistName,"*")){
       banner += firstHistName;
       banner += "\"";
   }
   else
       banner =  "first histogram";

   pl = new TPaveLabel(0.06, 0.3, 0.9, 0.45, banner.Data(),"br");
   pl->SetFillColor(18);
   pl->SetTextSize(0.7);
   pl->Draw();
   
   banner =  "by: \"";
   if (strcmp(lastHistName,"*")){
       banner += lastHistName;
       banner += "\"";
   }
   else
       banner =  "the end of list";

   pl = new TPaveLabel(0.06,0.06,0.92,0.2,banner.Data(),"br");
   pl->SetFillColor(18);
   pl->SetTextSize(0.7);
   pl->Draw();
   QACanvas->Modified();
   QACanvas->Update();

//   delete QACanvas;

   p5->NewPage();

   QACanvas = new TCanvas("CanvasName","Canvas Title",30*width,30*height);
   QACanvas->SetFillColor(19);
   QACanvas->SetBorderSize(2);
   QACanvas->Divide(xSize,ySize);

   p5->NewPage();

   Int_t padCount = 0;
  //_____________________
  // Create an itertor
    TIter nextKey(keys);
    TKey *key = 0; 
    Int_t histCounter = 0;
    Int_t histReadCounter = 0;
    Bool_t started = kFALSE;
    while (key = (TKey *)nextKey()) {
      TObject *obj = key->ReadObj();
      if (obj && obj->InheritsFrom("TH1")) { 
          histReadCounter++;
          printf(" %d. Reading ... %s::%s; Title=\"%s\"\n",histReadCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
          if (! started && (strcmp("*",firstHistName)==0 || strcmp(obj->GetName(),firstHistName)==0 ))  started = kTRUE;
          if (started) {
            if (strcmp(obj->GetName(),lastHistName)==0) started = kFALSE;
            histCounter++;
            printf("  -   %d. Drawing ... %s::%s; Title=\"%s\"\n",histCounter,obj->ClassName(),obj->GetName(), obj->GetTitle());
            if (padCount == numPads) {
                p5->NewPage();
                padCount=0;
            }
            QACanvas->cd(++padCount);

            obj->Draw();   
            if (gPad) {
              gPad->Modified();
              gPad->Update();
           }
         }
      }
    } 
  }
  p5.Close();
  printf(" ---------------------------------------------------------------------------\n");
  printf(" %d histograms have been plotted from file: \"%s\"\n", histCounter,fileName);
  printf(" The postscript copy of them have been created: \"%s\"\n",psFile);
  printf(" ---------------------------------------------------------------------------\n");
  printf(" One may use ROOT browser to investigate some particular histogram now\n");
  if (QABrowser) delete QABrowser;
  QABrowser = new TBrowser;
  delete [] exFileName;
  delete [] exPsFile;
}
