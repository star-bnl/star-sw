// $Id: StFtpcTrack.hh,v 1.1 2000/05/10 13:39:23 oldi Exp $
// $Log: StFtpcTrack.hh,v $
// Revision 1.1  2000/05/10 13:39:23  oldi
// Initial version of StFtpcTrackMaker
//

/////////////////////////////////////////////////////////////////////////////////
//                                                                             //
// StFtpcTrack class - representation of one FTPC track for the FTPC trackers. //
//                                                                             //
/////////////////////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcTrack
#define STAR_StFtpcTrack

#include "TObject.h"
#include "TObjArray.h"
#include "TClonesArray.h"

#include "MIntArray.h"

#include "StThreeVector.hh"
#include "fpt_fptrack.h"

class StFtpcVertex;

class StFtpcTrack : public TObject {
  
private:
  
  // data from tracker
  TObjArray *mPoints;         // Array of pointers to clusters of track
  MIntArray *mPointNumbers;   // Array of numbers of clusters
  
  Double_t   mRadius;         // Radius of the helix (projected to a circle)
  Double_t   mCenterX;        // x coordinate of the center of the helix (projected to a circle)
  Double_t   mCenterY;        // y coordinate of the center of the helix (projected to a circle)
  Double_t   mAlpha0;         // angle between first point of track (may be the vertex) and x axis as seen from center of circle projetion
  Int_t      mPid;            // particle id
  Short_t    mNMax;           // number of possible hits on this track
  Bool_t     mFromMainVertex; // true if tracks origin is the main vertex, otherwise false
  
  // data from momentum fit
  StThreeVector<double>   mP;        // ThreeVector of track momentum
  StThreeVector<double>   mV;        // ThreeVector of vertex used in fit
                  Int_t   mQ;        // charge measured in fit 
               Double_t   mChiSq[2]; // Chi2 of momentum fit
               Double_t   mTheta;    // theta value of momentum fit
               Double_t   mDca;      // radial impact parameter to main vertex

public:
  
              StFtpcTrack();  // constructor
              StFtpcTrack(fpt_fptrack_st *track_st, TClonesArray *hits = 0);  // constructor if STAF track is given
    virtual  ~StFtpcTrack();  // destructor
       void   Fit();                                        // momentum fit
       void   Fit(StFtpcVertex *vertex, Double_t max_Dca);  // momentum fit with vertex
       void   CalculateNMax();                              // calculates the max. possible number of points
   Double_t   CalcDca(StFtpcVertex *vertex);                // calculation of distance of closest approach (dca) to main vertex
      Int_t   Write(fpt_fptrack_st *trackTableEntry);       // writes track to table

  // getter
              TObjArray  *GetHits()             const { return mPoints;                   }
              MIntArray  *GetHitNumbers()       const { return mPointNumbers;             }
               Double_t   GetRadius()           const { return mRadius;                   }
               Double_t   GetCenterX()          const { return mCenterX;                  }
               Double_t   GetCenterY()          const { return mCenterY;                  }
               Double_t   GetAlpha0()           const { return mAlpha0;                   }
               Double_t   GetPid()              const { return mPid;                      }
                Short_t   GetNMax()             const { return mNMax;                     }
                  Int_t   GetNumberOfPoints()   const { return mPoints->GetEntriesFast(); }
                 Bool_t   ComesFromMainVertex() const { return mFromMainVertex;           }
  StThreeVector<double>   GetMomentum()         const { return mP;                        }
               Double_t   GetPx()               const { return mP.x();                    }
               Double_t   GetPy()               const { return mP.y();                    }
               Double_t   GetPz()               const { return mP.z();                    }
               Double_t   GetP() const;
               Double_t   GetPt() const;
               Double_t   GetPseudoRapidity() const;
               Double_t   GetRapidity() const;
  
  StThreeVector<double>   GetVertex()           const { return mV;                        }
                  Int_t   GetCharge()           const { return mQ;                        }
         Double_t const  *GetChiSq()            const { return mChiSq;                    }
               Double_t   GetTheta()            const { return mTheta;                    }
               Double_t   GetDca()              const { return mDca;                      }

  // setter
            void   SetPx(Double_t f)             {          mP.setX(f); }
            void   SetPy(Double_t f)             {          mP.setY(f); }
            void   SetPz(Double_t f)             {          mP.setZ(f); }

            void   SetRadius(Double_t f)         {        mRadius  = f; }
            void   SetCenterX(Double_t f)        {        mCenterX = f; }
            void   SetCenterY(Double_t f)        {        mCenterY = f; }
            void   SetAlpha0(Double_t f)         {         mAlpha0 = f; }
            void   SetCharge(Int_t f)            {              mQ = f; }
            void   SetPid(Int_t f)               {            mPid = f; }
            void   SetNMax(Short_t f)            {           mNMax = f; }
            void   ComesFromMainVertex(Bool_t f) { mFromMainVertex = f; }

            void   SetProperties(Bool_t fUsage, Int_t mTrackNumber);  

  ClassDef(StFtpcTrack, 1)    // Ftpc track class  
};

#endif
