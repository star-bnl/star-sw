// TreeEntryClasses.h
// M.L. MIller, Yale Software, 7/00

#ifndef TreeEntryClasses_h
#define TreeEntryClasses_h

#include "TObject.h"
#include "TStyle.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TFile.h"

class StRichTrack;
class StEvent;
class StRichMaterialsDb;


class HitEntry : public TObject {
public:
    HitEntry();
    HitEntry(Float_t , Float_t, Float_t, Float_t, Float_t, Int_t, Int_t, Int_t, Int_t);
    
    virtual ~HitEntry();
  
            
private:
  float x;
  float y;
  float d;
  float sig;
  float ang;
  int ring;
  int adc;
  int npads;
  int nhits;      
  ClassDef(HitEntry,1)
};


class TrackEntry : public TObject {
public:
    TrackEntry();
    TrackEntry(const TrackEntry&);
    virtual ~TrackEntry();
    
    //Methods
    void clear();
    void copyTrack(const TrackEntry&);
    void print() const;
    void copyHits(const TrackEntry& t);

    void setP(Float_t );
    Float_t getP() const;
  
    void addTrackInfo(StRichTrack*, StEvent*, StRichMaterialsDb*, Int_t, Int_t);
  
    void addHit(HitEntry);
  
    int getHitCounter() const;
  
    //Access
    const TClonesArray* getHitArray() const;
  
private:


  double vertexz;
  double eventrun;
  double eventn;
  double refit;
  double q;
  double nrichtracks;
  double nnegtracks;

  double curvature;
  double pdca;
  double gdca;

  double gpx;
  double gpy;
  double gpz;

  double lpx;
  double lpy;
  double lpz;
  
  double pmipx;
  double pmipy;
  
  double amipx;
  double amipy;
  double amipq;
  double amipn;
  double amipmax;
  
  double radx;
  double rady;
  
  double lastx;
  double lasty;
  double lastz;
  
  double firstx;
  double firsty;
  double firstz;
  
  
  double fitpoints;
  
  double indexin;
  double indexout;
  
  double picons1sig;
  double picons2sig;
  double piconsang;
  double piconsazi;
  double piconsarea;
  double pitotazi;

  double kacons1sig;
  double kacons2sig;
  double kaconsang;
  double kaconsazi;
  double kaconsarea;
  double katotazi;

  double prcons1sig;
  double prcons2sig;
  double prconsang;
  double prconsazi;
  double prconsarea;
  double prtotazi;

  int hitCounter;

  TClonesArray* hitArray;

  ClassDef(TrackEntry,1)
};

#endif
