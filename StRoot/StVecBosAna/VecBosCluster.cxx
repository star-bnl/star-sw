
#include "VecBosCluster.h"

#include "VecBosEvent.h"
#include "VecBosTrack.h"


VecBosCluster::VecBosCluster() : TObject(),
   mEnergy(0), ET(0), adcSum(0), nTower(0),
   iEta(0), iPhi(0), position(), mSize(1),
   mTowerBand1(), mTowerBand2(), mTowerBand3()
{
   Clear();
}


VecBosCluster::VecBosCluster(UShort_t size) : TObject(),
   mEnergy(0), ET(0), adcSum(0), nTower(0),
   iEta(0), iPhi(0), position(), mSize(size),
   mTowerBand1(), mTowerBand2(), mTowerBand3()
{
   Clear();
}


VecBosCluster::~VecBosCluster()
{ }


void VecBosCluster::BuildAroundTower(VecBosTrack &track)
{
   UShort_t towerEtaBin = track.mMatchedTower.GetEtaBin();
   UShort_t towerPhiBin = track.mMatchedTower.GetPhiBin();

   VecBosEvent  *vbEvent  = track.mEvent;
   VecBosVertex const *vbVertex = track.mVertex;

   iEta = towerEtaBin;
   iPhi = towerPhiBin;

   TVector3 cluCoord;
   double sumW          = 0;
   float  nomBTowRadius = gBTowGeom->Radius();

   for (int iEtaBin = towerEtaBin-1; iEtaBin <= towerEtaBin+1+mSize-1; iEtaBin+=mSize+1)
   {
      // trim in eta-direction
      if (iEtaBin < 0 || iEtaBin >= mxBTetaBin) continue;

      for (int iPhiBin = towerPhiBin-1; iPhiBin < towerPhiBin+1+mSize-1; iPhiBin+=mSize+1)
      {
         // Wrap up in the phi-direction
         iPhiBin = (iPhiBin + mxBTphiBin) % mxBTphiBin;  // keep it always positive
         int   towerId = gMapBTowEtaPhiBin2Id[ iEtaBin + iPhiBin * mxBTetaBin];
         float energy  = vbEvent->bemc.eneTile[kBTow][towerId - 1];

         //if (L<5) printf("n=%2d  iEtaBin=%d iPhiBin=%d\n", cluster.nTower, iEtaBin, iPhiBin);

         if (energy <= 0) continue; // skip towers w/o energy

         float adc    = vbEvent->bemc.adcTile[kBTow][towerId - 1];
         float deltaZ = gBCalTowerCoords[towerId - 1].Z() - vbVertex->mPosition.Z();
         float cosine = nomBTowRadius / sqrt(nomBTowRadius * nomBTowRadius + deltaZ * deltaZ);
         float ET     = energy * cosine;
         float logET  = log10(ET + 0.5);

         nTower++;
         mEnergy += energy;
         ET      += ET;
         adcSum  += adc;

         if (logET > 0) {
            cluCoord += logET * gBCalTowerCoords[towerId - 1]; // (log) energy weighted cluster position
            sumW     += logET;
         }
         // if(etaWidth==2)
         //    printf("towerEtaBin=%d towerPhiBin=%d  ET=%.1f  energy=%.1f   sum=%.1f logET=%f sumW=%f\n",
         //            iEtaBin, iPhiBin, ET, energy, cluster.mEnergy, logET, sumW);
      }

      // printf(" end btowSquare: towerEtaBin=%d  nTw=%d, ET=%.1f adc=%.1f\n",iEtaBin,cluster.nTower,cluster.ET,cluster.adcSum);
      if (sumW > 0)
         position = (1. / sumW) * cluCoord; // weighted cluster position
      else
         position = TVector3(0, 0, 999);
   }
}


void VecBosCluster::Clear(const Option_t* opt)
{
   position = TVector3(0, 0, 0);
   ET = mEnergy = adcSum = 0;
   nTower = 0;
   iEta = iPhi = 999;
}


void VecBosCluster::Print(const Option_t* opt) const
{
   if (string(opt).size())
      Info("Print", "opt: %s", opt);

   Double_t position_0to2Pi = position.Phi() < 0 ? 2*M_PI + position.Phi() : position.Phi();

   Info("Print", "E=%.1f GeV, ET=%.1f GeV, sumAdc=%.0f, nTw=%d, iEta=%d, iPhi=%d, XYZ: %.2f, %.2f, %.2f cm, Eta: %.2f, Phi: %.2f\n",
          ET, mEnergy, adcSum, nTower, iEta, iPhi, position.x(), position.y(), position.z(), position.PseudoRapidity(), position_0to2Pi);
   //position.Print();
}


