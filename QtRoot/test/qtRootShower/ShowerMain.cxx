#include "ShowerMain.h"
#include "RootShower.h"
#ifdef WIN32
# include "TROOT.h"
#endif
#include <TStyle.h>
#include <TROOT.h>
#include <TRandom.h>
#include <time.h>


//______________________________________________________________________________
void  ShowerMain::main(/*int argc, char **argv*/)
{
    //Bool_t rint = kFALSE;
    //for (int i = 0; i < argc; i++) {
    //   if (!strcmp(argv[i], "-d")) rint = kTRUE;
    //   if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "-?")) {
    //      printf("Usage: %s [-d] [-h | -?]\n", argv[0]);
    //      printf("  -d:     debug and inspect mode via ROOT prompt\n");
    //      printf("  -h, -?: this message\n");
    //      return 0;
    //   }
    //}

    //TApplication *theApp;
    //if (rint)
    //   theApp = new TRint("App", &argc, argv);
    //else
    //   theApp = new TApplication("App", &argc, argv);

    gStyle->SetOptStat(1111);
    gStyle->SetOptFit(1111);
    gStyle->SetStatFont(42);

    gRandom->SetSeed( (UInt_t)time( NULL ) );
#if ROOT_VERSION_CODE < ROOT_VERSION(5,16,0)
    const Int_t NRGBs = 5;
    Double_t Stops[NRGBs] = { 0.00, 0.34, 0.61, 0.84, 1.00 };
    Double_t Red[NRGBs] = { 1.00, 0.75, 0.50, 0.25, 0.00 };
    Double_t Green[NRGBs] = { 0.00, 0.00, 0.00, 0.00, 0.00 };
    Double_t Blue[NRGBs] = { 0.00, 0.25, 0.50, 0.75, 1.00 };
    gColIndex = gStyle->CreateGradientColorTable(NRGBs, Stops, Red, Green, Blue, 11);
#endif
    // Create RootShower

   gROOT->SetLineHasBeenProcessed();
    // Create RootShower
   fThisShower = new RootShower(0, 600, 500);
//    fThisShower->resize(400,200);
    show();
}
//______________________________________________________________________________
void ShowerMain::show()
{ fThisShower->show();} 

