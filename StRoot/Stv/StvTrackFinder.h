/// \File StvTrackFinder.h
/// \author Victor Perev 01/2010
#ifndef StvTrackFinder_HH
#define StvTrackFinder_HH
#include "StvStl.h"
#include "TNamed.h"

/// \class StvTrackFinder
class StvHit;
class StvDraw;
class StvPoints;
class StvHits;
class StvKonst_st;
class StvTrackFitter;
class StvTrackFinder : public TNamed
{
public:
  StvTrackFinder(const char *name):TNamed(name,""){fDraw=0;fDoShow=0;mRefit=1;}
  virtual ~StvTrackFinder();
  virtual int       FindTracks()			=0;
  virtual int	    FindPrimaries(const StvHits &vtxs)	=0;
  virtual void      Reset()				=0;
  virtual void      Clear(const char *opt="");
  virtual void      SetCons(const StvKonst_st*)=0;
  virtual void      SetFitter(StvTrackFitter *fitter){mTrackFitter = fitter;}
          void      AddPoint(const double pt[3]);
          void      AddHits(const double pt[3]);
  virtual StvNode *MakeDcaNode(StvTrack *tk)=0;
          void      Show();
          void      DoShow(int lev);
          int       DoShow() const {return fDoShow;};
          void SetRefit(int r=1)  {mRefit = r;} 
  
protected:
static StvDraw *NewDraw();
protected:
StvTrackFitter *mTrackFitter;
int  mRefit; 	//refit flag
int fDoShow;
StvDraw *fDraw;
StvPoints fShowTrak;
StvHits   fShowTrakHits;
StvHits   fShowFreeHits;
private:

ClassDef(StvTrackFinder,0);
};


#endif
