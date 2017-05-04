// $Id: StXTrakTestMaker.cxx,v 1.1 2017/05/04 01:22:15 perev Exp $
/// \File StXTrakTestMaker.cxx
/// \author V.Perev 2016
//
/*!

\class StXTrakTestMaker

A maker StXTrakTestMaker is a auxiliary maker for Sti/StiCA/Stv packages.
<br>
Main tasks:
<ul>
<li> Xtend/prolong StTrack to far detector;
<li> Save produced data into StEvent.
</ul>
*/
#include <Stiostream.h>
#include <math.h>
#include <string>
#include "TSystem.h"
#include "TVector3.h"
#include "TH2F.h"
#include "TCanvas.h"
#include "StDetectorId.h"
#include "StEventTypes.h"
#include "StEvent/StTrack.h"
#include "StEvent/StPrimaryTrack.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StExtGeometry.h"
#include "StEvent/StContainers.h"
#include "StEvent/StVertex.h"
#include "StThreeVectorD.hh"

#include "StXTrakTestMaker.h"
#include "StEvent/StEventSummary.h"

ClassImp(StXTrakTestMaker)

//_____________________________________________________________________________
StXTrakTestMaker::StXTrakTestMaker(const Char_t *name) :
    StMaker(name)

{
}

//_____________________________________________________________________________
StXTrakTestMaker::~StXTrakTestMaker()
{
  cout <<"StXTrakTestMaker::~StXTrakTestMaker() -I- Started/Done"<<endl;
}

//_____________________________________________________________________________
void StXTrakTestMaker::Clear(const char*)
{
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StXTrakTestMaker::Finish()
{
  int n = 0;
  for (auto it = mMap.begin(); it!=mMap.end(); ++it) {
    TH1* h= (*it).second;

//    printf("%d == %s\n",n++,h->GetName());
   auto *C = new TCanvas(h->GetName(),h->GetName(),600,800);
   C->cd(); h->Draw();
  }
  return 0;
}

//_____________________________________________________________________________
Int_t StXTrakTestMaker::Init()
{
  return 0;
}
//_____________________________________________________________________________
TH2 *StXTrakTestMaker::GetTH2(const char *name)
{
  TH2* &th = (TH2*&)mMap[name];
  if (!th) th = new TH2F(name, name, 100, 0,0,100,0,0);
  return th;
}

//_____________________________________________________________________________
Int_t StXTrakTestMaker::Make()
{

  StEvent   * event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (!event) return kStWarn;
  const StSPtrVecTrackNode& nodes= event->trackNodes();
  int nNodes = nodes.size();
  for (int iNode = 0;iNode <nNodes; iNode++) {
//  ===================================================
    StTrackNode *node = nodes[iNode];

    for (int iprim =0;iprim<2;iprim++) {

      StTrack *track = (iprim)? node->track(primary):node->track(global);
      if (!track)  continue;
      const StExtGeometry *xg = track->extGeometry();
      if (!xg) continue;		//No extention
      do {// Loop along the track extentions
	TString name(xg->name());
	name+=  (iprim)? "_Prim":"_Glob";
	double Rxy = xg->rxy();
	double Z   = xg->z();
	double L   = xg->length();
	double Phi = xg->phi();

	 TString ts;
	 ts = name; ts+="_Rxy_vs_Z";      
	 GetTH2(ts)->Fill(Z,Rxy);
	 ts = name; ts+="_Len_vs_Z";      
	 GetTH2(ts)->Fill(Z,L);

	 ts = name; ts+="_Rxy_vs_Phi";      
	 GetTH2(ts)->Fill(Phi*67,Rxy);
	 ts = name; ts+="_Len_vs_Phi";      
	 GetTH2(ts)->Fill(Phi*57,L);
	 ts = name; ts+="_Y_vs_X";      
	 GetTH2(ts)->Fill(Rxy*cos(Phi),Rxy*sin(Phi));
      } while((xg = xg->next()));	//end Xtentions
    } //End Glob/Prim
  }//End stnodes
  return kStOK;
}

