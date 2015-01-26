#ifndef HitMatchT_h
#define HitMatchT_h

#include "Riostream.h"
#include "TObject.h"
#include "TString.h"
class HitMatchT : public TObject
{
public:
   Char_t start;
   UInt_t index2Track;
   UInt_t index2Hit;
   UInt_t detId;  // hit detector Id
   UInt_t nRawHits;
   Double32_t xGP, yGP, zGP;   // Prediction in Global CS
   Double32_t dxGP, dyGP, dzGP; // global direction
   Double32_t xTPCP, yTPCP, zTPCP;   // Prediction in TPC CS
   Double32_t dxTPCP, dyTPCP, dzTPCP; // direction in TPC CS
   Double32_t xLP, yLP, zLP;  // aka (u,w,v)
   Double32_t tuP, tvP;
   Double32_t xG, yG, zG;    // hit Global from StEvent
   Double32_t xTPC, yTPC, zTPC;    // hit in TPC CS
   Double32_t xL, yL, zL;    // hit in Ladder CS
   Double32_t pT, eta, phi;  // track mom at origin (dcaGeometry)
   Double32_t ox, oy, oz;    // origin (dcaGeometry)
   Double32_t wGu, wGv, wGw; // Global direction for detector plane
   Int_t      npoint;        // npoint
   Double32_t firstPointR;   // first measured point R
   Double32_t firstPointZ;   // first measured point Z
   Int_t      NM;            // no. of best matchs for the given track
   Int_t      SectorTpc;     //
   Char_t end;

public:

   HitMatchT() {
      memset(&start, 0, &end - &start);
   }
   virtual ~HitMatchT() {}

   void Clear(Option_t *option = "") {if (option); memset(&start, 0, &end - &start);}
   void Set(Double32_t X, Double32_t Y, Double32_t Z,
            Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
      xG = X; yG = Y; zG = Z;
      xL = XL; yL = YL; zL = ZL;
   }
   void SetPred(Double32_t X, Double32_t Y, Double32_t Z,
                Double32_t XL = 0, Double32_t YL = 0, Double32_t ZL = 0) {
      xGP = X; yGP = Y; zGP = Z;
      xLP = XL; yLP = YL; zLP = ZL;
   }
   void SetNM(Int_t m=0) {NM = m;}
   void SetDetId(UInt_t id) {detId = id;}
   void SetNRawHits(UInt_t n) {nRawHits = n;}
   void Set(Double32_t *xyzG, Double32_t *xyzL) {Set(xyzG[0], xyzG[1], xyzG[2], xyzL[0], xyzL[1], xyzL[2]);}
   void SetTPC(Double32_t *xyzTPC) {xTPC = xyzTPC[0]; yTPC = xyzTPC[1]; zTPC = xyzTPC[2];}
   void SetPred(Double32_t *xyzG, Double32_t *xyzL) {SetPred(xyzG[0], xyzG[1], xyzG[2], xyzL[0], xyzL[1], xyzL[2]);}
   void SetPredDir(Double32_t *dirGPred) {dxGP = dirGPred[0]; dyGP = dirGPred[1]; dzGP = dirGPred[2];}
   void SetPredTPC(Double32_t *xyzTPCP) {xTPCP = xyzTPCP[0]; yTPCP = xyzTPCP[1]; zTPCP = xyzTPCP[2];}
   void SetPredDirTPC(Double32_t *dirTPCPred) {dxTPCP = dirTPCPred[0]; dyTPCP = dirTPCPred[1]; dzTPCP = dirTPCPred[2];}
   void SettuvPred(Double32_t tu, Double32_t tv) {tuP = tu; tvP = tv;}

   void SetIndex2Track(UInt_t index) {index2Track = index;}
   void SetIndex2Hit(UInt_t index) {index2Hit = index;}

   void SetTrackMom(Double32_t t_pT, Double32_t t_eta, Double32_t t_phi) {pT = t_pT; eta = t_eta; phi = t_phi;}
   void SetTrackOrigin(Double32_t x, Double32_t y, Double32_t z) {ox = x; oy = y; oz = z;}
   void SetTrackNpoint(Int_t n) {npoint = n;}
   void SetTrackFirstPointR(Double32_t r) {firstPointR = r;}
   void SetTrackFirstPointZ(Double32_t z) {firstPointZ = z;}
   void SetWG(Double32_t wu, Double32_t wv, Double32_t ww) { wGu = wu; wGv = wv; wGw = ww;}
   Double_t Diff() const  {return TMath::Sqrt((xL - xLP)*(xL - xLP) + (yL - yLP)*(yL - yLP));}
   // i_ladder[1-N], i_sector[1-10], i_sensor[1-N]
   //    Pxl =  (i_sector-1) * 40 + (i_ladder-1) * 10 + i_sensor
   //    Ist =               1000 + (i_ladder-1) *  6 + i_sensor; 
   //    Sst =               2000 + (i_ladder-1) * 16 + i_sensor;
   Int_t Sector() const  {
     if (detId > 1000) return 0;
     return (detId - 1)/40 + 1;
   }
   Int_t Ladder() const  {
     if (detId > 1000) return (detId - 1001)/6 + 1;
     return (detId - 40*(Sector() - 1)  - 1)/10 + 1;} 
   Int_t Sensor() const  {
     if (detId > 1000) return (detId - 1001)%6 + 1;
     return (detId - 40*(Sector() - 1) - 10*Ladder());}
   Int_t Layer() const  {
     if (detId > 1000) return 3;
     if ((Ladder()-1) % 4 == 0) return 1; 
     return 2;
   }
   Int_t Half() const  {
     if (detId > 1000) return -1;
     return (Sector()-1)/5 + 1;
   }
   virtual void      Print(Option_t *opt = "") const {
     cout << "HitMatchT\t" << detId 
	  << " Layer = "   << Layer() 
	  << " Sector = "  << Sector() 
	  << " Ladder = "  << Ladder() 
	  << " Sensor = "  << Sensor() 
	  << Form(" G: %8.3f %8.3f %8.3f",xG,yG,zG)  
	  << Form(" GP: %8.3f %8.3f %8.3f",xGP,yGP,zGP)  
	  << Form(" TPC: %8.3f %8.3f %8.3f",xTPC,yTPC,zTPC)  
	  << Form(" TPCP: %8.3f %8.3f %8.3f",xTPCP,yTPCP,zTPCP)  
	  << " Diff = "    << Diff() << endl;
   }
   ClassDef(HitMatchT, 4)
};

#endif
