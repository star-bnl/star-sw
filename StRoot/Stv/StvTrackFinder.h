/// \File StvTrackFinder.h
/// \author Victor Perev 01/2010
#ifndef StvTrackFinder_HH
#define StvTrackFinder_HH
#include "StvStl.h"
#include "TNamed.h"

/// \class StvTrackFinder
class StvKonst_st;
class StvTrackFitter;
class StvTrackFinder : public TNamed
{
public:
  StvTrackFinder(const char *name):TNamed(name,""){mRefit=1;}
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
          void SetRefit(int r=1)  {mRefit = r;} 
  
protected:
StvTrackFitter *mTrackFitter;
int  mRefit; 	//refit flag
private:

ClassDef(StvTrackFinder,0);
};


#endif
