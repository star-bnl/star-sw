// $Id: StFtpcTrack.hh,v 1.6 2000/07/18 21:22:16 oldi Exp $
// $Log: StFtpcTrack.hh,v $
// Revision 1.6  2000/07/18 21:22:16  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.5  2000/07/12 11:58:43  jcs
// calculate and save FTPC track parameters for unconstrained fit
//
// Revision 1.4  2000/07/03 12:42:57  jcs
// save (pre)Vertex id and unconstrained fit results
//
// Revision 1.3  2000/06/07 11:43:30  oldi
// New data members added: mRowsWithPoints, mChi2Circle, mChi2Length, mRFirst, mRLast, mAlphaFirst, mAlphaLast.
// Added the getters and setters for the new data members.
// Added GetEta().
//
// Revision 1.2  2000/05/11 15:14:51  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
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
#include "StFtpcPoint.hh"

#include "StThreeVector.hh"
#include "fpt_fptrack.h"

class StFtpcPoint;
class StFtpcVertex;

class StFtpcTrack : public TObject {
  
private:
  
  // data from tracker
  TObjArray *mPoints;         // Array of pointers to clusters of track
  MIntArray *mPointNumbers;   // Array of numbers of clusters
      Int_t  mRowsWithPoints; // 

     Int_t   mTrackNumber;    // number of track
  Double_t   mChi2Circle;     // Chi squared of circle fit
  Double_t   mChi2Length;     // Chi squared of length fit
  Double_t   mTrackLength;    // Length of track helix from first to last point
  Double_t   mRadius;         // Radius of the helix (projected to a circle)
  Double_t   mCenterX;        // x coordinate of the center of the helix (projected to a circle)
  Double_t   mCenterY;        // y coordinate of the center of the helix (projected to a circle)
  Double_t   mAlpha0;         // angle between first point of track (may be the vertex) and x axis as seen from center of circle projetion
     Int_t   mPid;            // particle id
   Short_t   mNMax;           // number of possible hits on this track
  Double_t   mRFirst;         // radius of (virtual) trackpoint in the first (inner) pad row
  Double_t   mRLast;          // radius of (virtual) trackpoint in the last (outer) pad row
  Double_t   mAlphaFirst;     // angle of (virtual) trackpoint in the first (inner) pad row
  Double_t   mAlphaLast;      // angle of (virtual) trackpoint in the last (outer) pad row
    Bool_t   mFromMainVertex; // true if tracks origin is the main vertex, otherwise false
  
  // data from momentum fit
  StThreeVector<double>   mP;        //! ThreeVector of track momentum
  StThreeVector<double>   mV;        //! ThreeVector of vertex used in fit
                  Int_t   mQ;        // charge measured in fit 
               Double_t   mChiSq[2]; // Chi2 of momentum fit
               Double_t   mTheta;    // theta value of momentum fit
               Double_t   mDca;      // radial impact parameter to main vertex

public:
  
              StFtpcTrack();                                                  // constructor
              StFtpcTrack(Int_t tracknumber);                                 // constructor which fills tracknumber (all other members are set to default values)
              StFtpcTrack(fpt_fptrack_st *track_st, TClonesArray *hits = 0, 
			  Int_t tracknumber = 0);                             // constructor if STAF track is given
    virtual  ~StFtpcTrack();                                                  // destructor

       void   SetDefaults();                                                  // performs the default setup for the track
       void   AddPoint(StFtpcPoint *point);                                   // adds a point to the track
       void   Fit();                                                          // momentum fit
       void   Fit(StFtpcVertex *vertex, Double_t max_Dca, Int_t id_start_vertex);                    // momentum fit with vertex
       void   CalculateNMax();                                                // calculates the max. possible number of points
   Double_t   CalcDca(StFtpcVertex *vertex);                                  // calculation of distance of closest approach (dca) to main vertex
   Double_t   CalcAlpha0();                                                   // calculation of the angle of xt with respect to the x axis
       void   CalcAndSetAlpha0() { this->SetAlpha0(this->CalcAlpha0()); }     // calculates and sets the angle of xt with respect to the x axis
      Int_t   Write(fpt_fptrack_st *trackTableEntry, Int_t id_start_vertex);  // writes track to table

  // getter
              TObjArray  *GetHits()             const { return mPoints;                          }
              MIntArray  *GetHitNumbers()       const { return mPointNumbers;                    }
                  Int_t   GetRowsWithPoints()   const { return mRowsWithPoints;                  }
                  Int_t   GetTrackNumber()      const { return mTrackNumber;                     }
               Double_t   GetChi2Circle()       const { return mChi2Circle;                      }
               Double_t   GetChi2Length()       const { return mChi2Length;                      }
               Double_t   GetRadius()           const { return mRadius;                          }
               Double_t   GetCenterX()          const { return mCenterX;                         }
               Double_t   GetCenterY()          const { return mCenterY;                         }
               Double_t   GetAlpha0()           const { return mAlpha0;                          }
               Double_t   GetPid()              const { return mPid;                             }
                Short_t   GetNMax()             const { return mNMax;                            }
               Double_t   GetRFirst()           const { return mRFirst;                          }
               Double_t   GetRLast()            const { return mRLast;                           }
               Double_t   GetAlphaFirst()       const { return mAlphaFirst;                      }
               Double_t   GetAlphaLast()        const { return mAlphaLast;                       }
                 Bool_t   GetHemisphere()       const { return (Bool_t)TMath::Sign(1., GetPz()); } 
                  Int_t   GetNumberOfPoints()   const { return mPoints->GetEntriesFast();        }
                 Bool_t   ComesFromMainVertex() const { return mFromMainVertex;                  }
  StThreeVector<double>   GetMomentum()         const { return mP;                               }
               Double_t   GetPx()               const { return mP.x();                           }
               Double_t   GetPy()               const { return mP.y();                           }
               Double_t   GetPz()               const { return mP.z();                           }
               Double_t   GetP() const;
               Double_t   GetPt() const;
               Double_t   GetPseudoRapidity() const;
               Double_t   GetEta() const; 
               Double_t   GetRapidity() const;
  
  StThreeVector<double>   GetVertex()           const { return mV;                               }
                  Int_t   GetCharge()           const { return mQ;                               }
         Double_t const  *GetChiSq()            const { return mChiSq;                           }
               Double_t   GetTheta()            const { return mTheta;                           }
               Double_t   GetDca()              const { return mDca;                             }

  // setter   
            void   SetTrackNumber(Int_t number);
            void   SetRowsWithPoints(Int_t f)    { mRowsWithPoints = f; }
	    
            void   SetPx(Double_t f)             {          mP.setX(f); }
            void   SetPy(Double_t f)             {          mP.setY(f); }
            void   SetPz(Double_t f)             {          mP.setZ(f); }

            void   SetChi2Circle(Double_t f)     {     mChi2Circle = f; }
            void   SetChi2Length(Double_t f)     {     mChi2Length = f; }
            void   SetRadius(Double_t f)         {         mRadius = f; }
            void   SetCenterX(Double_t f)        {        mCenterX = f; }
            void   SetCenterY(Double_t f)        {        mCenterY = f; }
            void   SetAlpha0(Double_t f)         {         mAlpha0 = f; }
            void   SetCharge(Int_t f)            {              mQ = f; }
            void   SetPid(Int_t f)               {            mPid = f; }
            void   SetRLast(Double_t f)          {          mRLast = f; }
            void   SetRFirst(Double_t f)         {         mRFirst = f; }
            void   SetAlphaLast(Double_t f)      {      mAlphaLast = f; }
            void   SetAlphaFirst(Double_t f)     {     mAlphaFirst = f; }

            void   SetDca(Double_t f)            {            mDca = f; }
            void   SetNMax(Short_t f)            {           mNMax = f; }
            void   ComesFromMainVertex(Bool_t f) { mFromMainVertex = f; }

            void   SetProperties(Bool_t fUsage, Int_t mTrackNumber);  
            void   SetPointDependencies();

  ClassDef(StFtpcTrack, 1)    // Ftpc track class  
};


inline Double_t StFtpcTrack::GetPt() const
{
  // Returns transverse momentum.

  return TMath::Sqrt(mP.x() * mP.x() + mP.y() * mP.y());
}


inline Double_t StFtpcTrack::GetP() const
{
  // Returns total momentum.

  return TMath::Sqrt(mP.x() * mP.x() + mP.y() * mP.y() + mP.z() * mP.z());
}


inline Double_t StFtpcTrack::GetPseudoRapidity() const
{
  // Returns the pseudo rapidity of the particle.

  return 0.5 * TMath::Log((GetP() + GetPz()) / (GetP() - GetPz()));  
}


inline Double_t StFtpcTrack::GetEta() const
{
  // This function returns the value of GetPseudoRapidity().
  
  return GetPseudoRapidity();
}


inline Double_t StFtpcTrack::GetRapidity() const
{
  // Returns the rapidity of the particle with the assumption that the particle is a pion (+/-).

  Double_t m_pi = 0.13957;

  return 0.5 * TMath::Log((m_pi + GetPz()) / (m_pi - GetPz()));
}


#endif
