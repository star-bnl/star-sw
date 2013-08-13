
#ifndef StiDraw_HH
#define StiDraw_HH
#include "StDraw3D.h"
class StiHit;
class THelixTrack;

class StiDraw : public StDraw3D
{
public: 

    ///Default constructor.
    StiDraw(const char *opt="TPC");
    ~StiDraw(){;}
   void  Clear(const char *opt="");
TObject *Hits(const std::vector<     StiHit*> &hits, EDraw3DStyle sty);
TObject *Hits(const std::vector<const float*> &hits, EDraw3DStyle sty);
TObject *Trak(const THelixTrack &helx,const std::vector<StiHit*>  &hits, EDraw3DStyle sty);
TObject *Trak(const std::vector<float> &pnts, EDraw3DStyle sty);
   void  DoIt();
static int ProcessEvents();
static StiDraw *Inst() {if (!fgStiDraw) fgStiDraw=new StiDraw(); return fgStiDraw;}
static StiDraw *Jnst() {return fgStiDraw;}
static void Wait();

private:
int mNDoIt;
int mNPow2;

static StiDraw *fgStiDraw;
};

#endif
