#ifndef VecBosCluster_h
#define VecBosCluster_h

#include "TLorentzVector.h"
#include "TVector3.h"


class VecBosTrack;


// Info BTOW cluster
class VecBosCluster : public TObject
{
public:

   float    mEnergy;
   float    ET;
   float    adcSum;
   int      nTower;       // with non-zero ADC>kSigPed
   int      iEta, iPhi;   // lower-left corner of the cluster, can be negative, L2-indexing convention
   TVector3 position;     // 3D ln(E) weighted sume of tower positions

   VecBosCluster();
   VecBosCluster(UShort_t size);
   ~VecBosCluster();

   void BuildAroundTower(VecBosTrack &track);
   virtual void Clear(const Option_t* opt="");
   virtual void Print(const Option_t* opt="") const;

protected:

   UShort_t       mSize;       //! size of the cluster, i.e. number of towers along each side, e.g. mSizexmSize
   TLorentzVector mTowerBand1; //! XXX remove ! 
   TLorentzVector mTowerBand2; //!
   TLorentzVector mTowerBand3; //!

   ClassDef(VecBosCluster, 1);
};

#endif
