/*!\class StEmcMicroTrack
\author Alexandre A. P. Suaide

This is the EMC micro track structure.
*/
 
#ifndef StEmcMicroTrack__h
#define StEmcMicroTrack__h
 
#include "TObject.h"
#include "math.h"
 
//-----------------------------------------------------------
 
class StEmcMicroTrack : public TObject 
{
 
public:
 
            StEmcMicroTrack();
            StEmcMicroTrack(StEmcMicroTrack*);
   virtual  ~StEmcMicroTrack();

   Float_t  getPt()                    const { return mP*sin(2*atan(exp(-mEta))); }  ///<Return PT of the track
   Float_t  getP()                     const { return mP; }                          ///<Return P of the track
   Float_t  getEta()                   const { return mEta; }                        ///<Return Eta of the track
   Float_t  getPhi()                   const { return mPhi; }                        ///<Return Phi of the track
   Float_t  getOrigin(Int_t i)         const { return mX[i]; }                       ///<Return Origin of the track (0=X, 1=Y, 2=Z)
   Float_t  getCurvature()             const { return mCurvature; }                  ///<Return Curvature of the track
   Short_t  getCharge()                const { return (Int_t)mCharge; }              ///<Return Charge of the track
   Float_t  getDca()                   const { return mDca; }                        ///<Return DCA of the track
   Float_t  getDcaSigned()             const { return mDcaSigned; }                  ///<Return DCA signed of the track
   Float_t  getChi2()                  const { return (Float_t)mChi2/1000.; }        ///<Return ChiSquare of the track
   Int_t    getFitPts()                const { return (Int_t)mFitPts; }              ///<Return Number of fit points of the track
   Int_t    getMaxPts()                const { return (Int_t)mMaxPts; }              ///<Return Max number of points of the track
   Int_t    getNhits()                 const { return (Int_t)mNhits; }               ///<Return Number of hits of the track
   Float_t  getDedx()                  const { return mDedx; }                       ///<Return dE/dX of the track
   Float_t  getDedxErr()               const { return mDedxErr; }                    ///<Return dE/dX error on mean value
   Int_t    getNdedxPts()              const { return (Int_t)mNdedxPts; }            ///<Return dE/dX number of points of the track
   Float_t  getTrackLength()           const { return mTrackLength; }                ///<Return track length
   Int_t    getTrackNode()             const { return mTrackNode; }                  ///<Return track node (useful for StEvent Reconstruction)
   Int_t    getFlag()                  const { return (Int_t)mFlag; }                ///<Return flag of the track

   void  setP(Float_t p)             { mP = p; }
   void  setEta(Float_t eta)         { mEta = eta; }
   void  setPhi(Float_t phi)         { mPhi = phi; }
   void  setCurvature(Float_t c)     { mCurvature = c; }
   void  setOrigin(Float_t x,Float_t y,Float_t z) { mX[0]=x;  mX[1]=y; mX[2]=z;}
   void  setCharge(Short_t charge)   { mCharge = (Char_t)charge; }
   void  setDca(Float_t dca)         { mDca = dca; }
   void  setDcaSigned(Float_t sdca)  { mDcaSigned = sdca; }
   void  setChi2(Float_t chi2)       { mChi2 = (Int_t)(chi2*1000.); }
   void  setFitPts(Int_t fitPts)     { mFitPts = (Char_t)fitPts; }
   void  setMaxPts(Int_t maxPts)     { mMaxPts = (Char_t)maxPts; }
   void  setNhits(Int_t nhits)       { mNhits = (Char_t)nhits; }
   void  setDedx(Float_t Dedx)       { mDedx = Dedx; }
   void  setDedxErr(Float_t err)     { mDedxErr = err; }
   void  setNdedxPts(Int_t ndedxPts) { mNdedxPts = (Char_t)ndedxPts; }
   void  setTrackLength(Float_t tl)  { mTrackLength = tl; }
   void  setTrackNodeNumber(Int_t n) { mTrackNode = n; } 
   void  setFlag(Int_t n)            { mFlag = (Char_t)n; } 
private:   
                                          
   Float_t   mP;                          // total momentum
   Float_t   mEta;                        // pseudorapidity
   Float_t   mPhi;                        // azimuthal angle
   Float_t   mCurvature;                  // track curvature
   Float_t   mX[3];                       // track origin (needed for projection)
   Char_t    mCharge;                     // charge
   Float_t   mDca;                        // distance of closest approach (? track model)
   Float_t   mDcaSigned;                  // 2D dca with sign (circle fit)
   Int_t     mChi2;                       // chi squared
   Char_t    mFitPts;                     // number of hits used in fit
   Char_t    mMaxPts;                     // maximum possible number of hits 
   Char_t    mNhits;                      // number of hits on the track
   Float_t   mDedx;                       // specific energy loss
   Float_t   mDedxErr;                    // dE/dX error on mean value
   Char_t    mNdedxPts;                   // number of hits use for dE/dx
   Float_t   mTrackLength;                // lenght of the track (cm)
   Int_t     mTrackNode;                  // original track node
   Char_t    mFlag;                       // flag
   ClassDef(StEmcMicroTrack,1)
};
 
#endif                       
