#include "StiTrackFinder.h"
#include "TSystem.h"
#include "StiDraw.h"
#include "vector"
StiTrackFinder *StiTrackFinder::fgInst=0;
ClassImp(StiTrackFinder)
//_____________________________________________________________________________
//_____________________________________________________________________________
StiTrackFinder::~StiTrackFinder()
{ DoShow(0);
  if (this == fgInst) fgInst=0;
}
//_____________________________________________________________________________
void StiTrackFinder::Show()
{
  if (!fDraw) return;
  if(fShowTrak.size())fDraw->Trak(fShowTrak,kGlobalTrack);
//if(fShowTrak.size())fDraw->Line(fShowTrak,kGlobalTrack);
  if(fShowTrakHits.size() )fDraw->Hits(fShowTrakHits ,kUsedHit  );
  if(fShowFreeHits.size() )fDraw->Hits(fShowFreeHits ,kUnusedHit);
  fShowTrak.clear();fShowTrakHits.clear();fShowFreeHits.clear();
  fDraw->UpdateModified();
//  fDraw->SetDrawOption("{view:all}");
  fDraw->ProcessEvents();
}
//_____________________________________________________________________________
StiDraw *StiTrackFinder::NewDraw()
{
   StiDraw *dr = new StiDraw();
   dr->SetBkColor(kWhite);
// dr->Style(kUsedHit).Siz()  *=2;
   dr->Style(kUnusedHit).Siz()*=20;
   return dr;
}
//_____________________________________________________________________________
void StiTrackFinder::DoShow(int lev)
{
  if (fDoShow == lev) return;
  fDoShow = lev;
  if (fDoShow) {if (!fDraw) fDraw=NewDraw();}
  else         { delete fDraw;fDraw=0 ;fShowTrak.clear();
                 fShowTrakHits.clear();fShowFreeHits.clear();}
}
//_____________________________________________________________________________
void StiTrackFinder::Clear(const char*)
{
if (fDraw)  fDraw->Clear();
fShowTrak.clear();
fShowTrakHits.clear();
fShowFreeHits.clear();
}
