
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
    StvDraw(const char *opt="TIFC,TPCFEE,TPCM");
    ~StvDraw(){;}
   void  Clear(const char *opt="");
TObject *Hits(const std::vector<      StvHit*> &hits, EDraw3DStyle sty);
TObject *Hits(const std::vector<const StvHit*> &hits, EDraw3DStyle sty);
TObject *Hits(const std::vector<const float*> &hits, EDraw3DStyle sty);

TObject *Trak(const THelixTrack &helx,const std::vector<const StvHit*>  &hits, EDraw3DStyle sty=kGlobalTrack);
TObject *Trak(const THelixTrack &helx,const std::vector<      StvHit*>  &hits, EDraw3DStyle sty=kGlobalTrack);
TObject *Trak(const std::vector<float> &pnts, EDraw3DStyle sty=kGlobalTrack,Color_t col=kRed);
   void  Trak(const StvTrack *tk, int dir = 2, EDraw3DStyle sty=kGlobalTrack);

   void  Road(const StvTrack *tk, double wide=5,EDraw3DStyle sty=kGlobalTrack);
   void  All(const char *opt);

   void  DoIt();
static int ProcessEvents();
static StvDraw *Inst() {if (!fgStvDraw) fgStvDraw=new StvDraw(); return fgStvDraw;}
static StvDraw *Jnst() {return fgStvDraw;}
static void Wait();
static void Show(const StvTrack *tk, int dir=0);
static void Zhow(const StvTrack *tk);
static void Klear();
static void KBrowse(const TObject *to);
private:
   void Join(const StvNode *left,const StvNode *rite,StvPoints &poits,int dir=2);
private:
int mNDoIt;
int mNPow2;
int mIColor;		//Current color index when it is changing in cycle 
static StvDraw *fgStvDraw;
};

#endif
