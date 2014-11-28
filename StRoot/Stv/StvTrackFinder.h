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
class StvTrackFinder : public TNamed
{
public:
  StvTrackFinder(const char *name):TNamed(name,""){fgInst=this;fDraw=0;fDoShow=0;}
  virtual ~StvTrackFinder();
  virtual int       FindTracks()			=0;
  virtual int	    FindPrimaries(const StvHits &vtxs)	=0;
  virtual void      Reset()				=0;
  virtual void      Clear(const char *opt="");
          void      AddPoint(const double pt[3]);
          void      AddHits(const double pt[3]);
          void      Show();
          void      DoShow(int lev);
          int       DoShow() const {return fDoShow;};
  
static StvTrackFinder *Inst() {return fgInst;}

protected:
static StvDraw *NewDraw();
protected:
int fDoShow;
StvDraw *fDraw;
StvPoints fShowTrak;
StvHits   fShowTrakHits;
StvHits   fShowFreeHits;
private:
static StvTrackFinder *fgInst;


ClassDef(StvTrackFinder,0);
};


#endif
