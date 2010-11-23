
#ifndef StvDraw_HH
#define StvDraw_HH
#include "StDraw3D.h"

class StvHit;
class StvNode;
class StvTrack;
class THelixTrack;
class StvPoints;
class StvDraw : public StDraw3D
{
public: 

    ///Default constructor.
    StvDraw(const char *opt="TPC");
    ~StvDraw(){;}
   void  Clear(const char *opt="");
TObject *Hits(const std::vector<     StvHit*> &hits, EDraw3DStyle sty);
TObject *Hits(const std::vector<const float*> &hits, EDraw3DStyle sty);
TObject *Trak(const THelixTrack &helx,const std::vector<StvHit*>  &hits, EDraw3DStyle sty=kGlobalTrack);
TObject *Trak(const std::vector<float> &pnts, EDraw3DStyle sty=kGlobalTrack);
   void  Trak(const StvTrack *tk, EDraw3DStyle sty=kGlobalTrack);
   void  DoIt();
static int ProcessEvents();
static StvDraw *Inst() {if (!fgStvDraw) fgStvDraw=new StvDraw(); return fgStvDraw;}
static StvDraw *Jnst() {return fgStvDraw;}
static void Wait();
static void Show(const StvTrack *tk);
private:
   void Join(const StvNode *left,const StvNode *rite,StvPoints &poits);
private:
int mNDoIt;
int mNPow2;

static StvDraw *fgStvDraw;
};

#endif
