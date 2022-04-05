/*!
 * \class StPmdCluster
 * \author
 */
/**************************************************************
 *
 * $Id: StPmdCluster.h,v 1.5 2004/06/24 13:53:41 subhasis Exp $
 *
 * Author:
 **************************************************************
 *
 * Description: Base class for PMD cluster
 *
 **************************************************************
*
* $Log: StPmdCluster.h,v $
* Revision 1.5  2004/06/24 13:53:41  subhasis
* sigmaL, sigmaS introduced
*
* 2004/06/24 : Cluster SigmaL and SigmaS included 
* according to new clustering algorithm. Number of
* cells changed to Float_t type  : Dipak
* Revision 1.4  2003/10/14 07:28:48  subhasis
* CluX, CluY added
*
* Revision 1.3  2003/09/02 17:58:49  perev
* gcc 3.2 updates + WarnOff
*
* Revision 1.2  2003/05/12 12:07:12  subhasis
* Mapping added
*
*
***************************************************************/
#ifndef STAR_StPmdCluster
#define STAR_StPmdCluster

#include <math.h>
#include <Stiostream.h>
#include "TArrayI.h"
#include "StObject.h"
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdHit.h"


class StPmdCluster : public StObject {

private:
  //! cluster objects
  Int_t             mModule;       // supermodule no
  //  Int_t             mNumofMems;    // no. of cells in the cluster
  Float_t             mNumofMems;    // no. of cells in the cluster
  Float_t             mCluEta;     // cluster eta
  Float_t             mCluPhi;     // cluster phi
  Float_t           mCluEdep;      // cluster edep
  Float_t           mCluSigmaL;     // sigma of the cluster along large axis
  Float_t           mCluSigmaS;     // sigma of the cluster along small axis
  Int_t             mCluPID;       // cluster PID based on matching
  Int_t             mCluEdepPID;   // clusterPID based on edep
  Int_t             mMcCluPID ;     // Mantecarlo cluster ID
  Float_t           mCluX;
  Float_t           mCluY; 
  TObjArray         mHitCollection;  // hits collection 
public: 
  StPmdCluster();      //! constructor     
  StPmdCluster(TArrayI*);   //! constructor
  ~StPmdCluster();          //! destructor

  /*! member functions*/

  Int_t           Module() const; //!for Supermodule number
  //  Int_t           NumofMems() const; //! number of cells in the cluster
  Float_t        NumofMems() const; //! number of cells in the cluster as float
  Float_t           CluEta() const; //! cluster eta
  Float_t           CluPhi() const;  //! cluster phi
  Float_t         CluEdep() const;  //! cluster edep
  Float_t        CluSigmaL() const;  //! cluster sigma Large
  Float_t        CluSigmaS() const;  //! cluster sigma Small
  Int_t         CluEdepPID() const;  //! cluster PID based on Edep
  Int_t         CluPID() const;   //! cluster PID based on CPV/PMD matching
  Int_t         McCluPID() const;  //! cluster PID based on Mantecarlo
  
  Float_t       CluX() const;
  Float_t       CluY() const; 
  
  TObjArray*    HitCollection();   //! hit collection

  void            setModule(Int_t);
  //  void            setNumofMems(Int_t);
 void            setNumofMems(Float_t); // Cell number as float
  void            setCluEta(Float_t);
  void            setCluPhi(Float_t);
  void            setCluEdep(Float_t);
  void            setCluSigmaL(Float_t); //Large
  void            setCluSigmaS(Float_t);//Small
  void            setCluEdepPID(Int_t);
  void            setCluPID(Int_t);
  void            setMcCluPID(Int_t);
  void            setCluX(Float_t);
  void            setCluY(Float_t);

  
  void            addHitCollection(StPmdHit*);

  virtual void      Browse(TBrowser *b);
  virtual void      print(ostream *os);

  ClassDef(StPmdCluster,1)// Base class for PMD cluster
};

ostream &operator<<(ostream&, StPmdCluster&); // Printing operator

inline              StPmdCluster::~StPmdCluster(){ /* Nobody */ }

inline   Int_t    StPmdCluster::Module() const {return mModule;}
//inline   Int_t    StPmdCluster::NumofMems() const {return mNumofMems;}
inline   Float_t    StPmdCluster::NumofMems() const {return mNumofMems;}
inline   Float_t    StPmdCluster::CluEta() const     {return mCluEta;} 
inline   Float_t    StPmdCluster::CluPhi() const    {return mCluPhi;}
inline   Float_t  StPmdCluster::CluEdep() const  {return mCluEdep;}
inline   Float_t  StPmdCluster::CluSigmaL() const  {return mCluSigmaL;}//Large
inline   Float_t  StPmdCluster::CluSigmaS() const  {return mCluSigmaS;} //Small
inline   Int_t  StPmdCluster::CluEdepPID() const  {return mCluEdepPID;}
inline   Int_t  StPmdCluster::CluPID() const  {return mCluPID;}
inline   Int_t  StPmdCluster::McCluPID() const  {return mMcCluPID;}
inline  Float_t  StPmdCluster::CluX() const {return mCluX;} 
inline  Float_t  StPmdCluster::CluY() const {return mCluY;}

inline   void    StPmdCluster::setModule(Int_t var) {mModule=var;}
//inline   void    StPmdCluster::setNumofMems(Int_t var) {mNumofMems=var;}
inline   void    StPmdCluster::setNumofMems(Float_t var) {mNumofMems=var;}
inline   void    StPmdCluster::setCluEta(Float_t var) {mCluEta=var;}
inline   void    StPmdCluster::setCluPhi(Float_t var) {mCluPhi=var;}
inline   void    StPmdCluster::setCluEdep(Float_t var) {mCluEdep=var;}
inline   void    StPmdCluster::setCluSigmaL(Float_t var) {mCluSigmaL=var;}//Large
inline   void    StPmdCluster::setCluSigmaS(Float_t var) {mCluSigmaS=var;}//Small
inline   void    StPmdCluster::setCluEdepPID(Int_t var) {mCluEdepPID=var;}
inline   void    StPmdCluster::setCluPID(Int_t var) {mCluPID=var;}
inline   void    StPmdCluster::setMcCluPID(Int_t var) {mMcCluPID=var;}
inline   void    StPmdCluster::setCluX(Float_t var) {mCluX=var;}
inline   void    StPmdCluster::setCluY(Float_t var) {mCluY=var;}

inline TObjArray* StPmdCluster::HitCollection() {return &mHitCollection;}
#endif







