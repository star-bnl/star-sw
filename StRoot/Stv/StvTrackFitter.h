/// \File StvTrackFitter.h
/// \author Victor Perev 9/2010
#ifndef StvTrackFitter_HH
#define StvTrackFitter_HH
#include "TNamed.h"

/// \class StvTrackFitter
class StvTrack;

class StvTrackFitter : public TNamed
{
public:
  StvTrackFitter(const char *name):TNamed(name,""){fgInst=this;Clear();}
  virtual ~StvTrackFitter()			{if(this==fgInst) fgInst=0;}
  virtual  int Refit(StvTrack *trak,int dir)	=0;
  virtual void Clear(const char *opt="")	{mNDF=0; mXi2=3e33;}
           int GetNDF() const 			{return mNDF;}     
        double GetXi2() const 			{return mXi2;}     
  
static StvTrackFitter *Inst() {return fgInst;}

protected:
int    mNDF;
double mXi2;
private:
static StvTrackFitter *fgInst;


ClassDef(StvTrackFitter,0);
};


#endif
