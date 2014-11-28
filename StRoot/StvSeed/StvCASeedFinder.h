/// \File StvCASeedFinder.h
/// \author Victorb Perev 01/2010
#ifndef StvCASeedFinder_HH
#define StvCASeedFinder_HH
#include "Stv/StvSeedFinder.h"
#include <map>
#include <vector>
/// \class StvCASeedFinder

class StvHit;
class AliHLTTPCCAParam;
class AliHLTTPCCAParamVector;
class AliHLTTPCCAGBHit;
class AliHLTTPCCAGBHitVector;
class IdTruthVector;
class AliHLTTPCCAGBTracker;
class StVoidArr;

class StvCASeedFinder : public StvSeedFinder
{
public:
  StvCASeedFinder(const char *name="TpcCaSeedFinder");
   ~StvCASeedFinder(){;}
  const THelixTrack* NextSeed();
  void      Clear(const char *opt="");
  void      Reset();
  void      Print(const char *opt="") const {;}

protected:

private:
  void Run();
  void MakeSettings();
  void MakeHits();

static Int_t padp(Int_t pad, Int_t row);
private:
char mBeg[1];
int fNTracks;
int fITrack;
StVoidArr *fStvHits;
char mMed[1];
AliHLTTPCCAGBTracker   *fTracker;
AliHLTTPCCAParamVector *fCaParam;	// settings for all sectors to give CATracker
AliHLTTPCCAGBHitVector *fCaHits; 	// hits to give CATracker
IdTruthVector          *fIdTruth; 	// id of the Track, which has created CaHit



char mEnd[1];
KlassDef(StvCASeedFinder,0);//K instead of C to avoid dictionary creation
};


#endif
