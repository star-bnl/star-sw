/***************************************************************************
 *                API between KUIP and ROOT                                *
 ***************************************************************************/

#include "TApplication.h"
#include "TStopwatch.h"
#include "TPaveLabel.h"
#include "TGeometry.h"
#include "TBrowser.h"
#include "TButton.h"
#include "TCanvas.h"
#include "TLine.h"
#include "TRint.h"
#include "TMath.h"
#include "TROOT.h"
#include "aroot.h"

//========================== C++  API ==================================

ClassImp(aroot)
extern "C" void kuexec_ (const char*,int);   //prototype
aroot s;  aroot &kuexec = s;                 //pointer to instance of aroot

aroot::aroot()
{  if (gApplication) return;
   int argc = 1;  const char *argv[] = {"audi"}; 
   gApplication = new TRint ("App",&argc,(char**) argv,0,0);
}
//----------------------------------------------------------------------
void aroot::kuexec (const char* command) 
{  kuexec_ (command, strlen(command)); }

//==========================FORTRAN API=================================

TCanvas        *can      = 0;
TPaveLabel     *hel      = 0;
TStopwatch     *wch      = 0;
extern "C" void root_() 
{  
   if (hel) 
   { hel->SetLabel("AUDI is active"); hel->Draw(); hel->SetBit(kCanDelete); }

   if (!wch) wch = new TStopwatch();
   gApplication->Run(kTRUE);          wch->Start(); wch->Print();  

   if (hel) { hel->SetLabel("AUDI is sleeping"); hel->Draw(); }
   if (can) { can->Modified(); can->Update(); }
} 
//----------------------------------------------------------------------
extern "C" void audi_()
{ 
   TButton *b1=0, *b2=0;
   if (!can)  can = new TCanvas   ("c","AUDI Geometry Converter",400,400);
   if (!hel)  hel = new TPaveLabel( 0.2, 0.4, 0.8, 0.6, " ");

   b1=new TButton("ag2root","Ag.ag2root();", .21,.35,.49,.42);
   b1->Draw();  b1->SetBit(kCanDelete); 

   b2=new TButton("ag2view","Ag.ag2view();", .51,.35,.80,.42);
   b2->Draw();  b2->SetBit(kCanDelete); 
   root_ ();
}
