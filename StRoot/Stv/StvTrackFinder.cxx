#include "StvTrackFinder.h"
#include "TSystem.h"
#include "StvDraw.h"
#include "vector"
ClassImp(StvTrackFinder)
//_____________________________________________________________________________
//_____________________________________________________________________________
StvTrackFinder::~StvTrackFinder()
{ DoShow(0);
}
//_____________________________________________________________________________
void StvTrackFinder::Show()
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
StvDraw *StvTrackFinder::NewDraw()
{
   StvDraw *dr = new StvDraw();
   dr->SetBkColor(kWhite);
// dr->Style(kUsedHit).Siz()  *=2;
   dr->Style(kUnusedHit).Siz()*=20;
   return dr;
}
//_____________________________________________________________________________
void StvTrackFinder::DoShow(int lev)
{
  if (fDoShow == lev) return;
  fDoShow = lev;
  if (fDoShow) {if (!fDraw) fDraw=NewDraw();}
  else         { delete fDraw;fDraw=0 ;fShowTrak.clear();
                 fShowTrakHits.clear();fShowFreeHits.clear();}
}
//_____________________________________________________________________________
void StvTrackFinder::Clear(const char*)
{
// if (fDraw)  fDraw->Clear();
// fShowTrak.clear();
// fShowTrakHits.clear();
// fShowFreeHits.clear();
}
