/*!
 * \class StPmdCluster
 * \author
 */
/**************************************************************
 *
 * $Id: StPmdCluster.h,v 1.1 2002/08/27 12:23:15 subhasis Exp $
 *
 * Author:
 **************************************************************
 *
 * Description: Base class for PMD cluster
 *
 **************************************************************
*
* $Log: StPmdCluster.h,v $
* Revision 1.1  2002/08/27 12:23:15  subhasis
* First version
*
*
***************************************************************/
#ifndef STAR_StPmdCluster
#define STAR_StPmdCluster

#include <math.h>
#include <iostream.h>
#include "TArrayI.h"
#include "StObject.h"
#include "StPmdUtil/StPmdGeom.h"
class StPmdCluster : public StObject {

private:
  //! cluster objects
  Int_t             mModule;       //! supermodule no
  Int_t             mNumofMems;    //! no. of cells in the cluster
  Float_t             mCluEta;     //! cluster eta
  Float_t             mCluPhi;     //! cluster phi
  Float_t           mCluEdep;      //! cluster edep
  Float_t           mCluSigma;     //! sigma of the cluster
  Int_t             mCluPID;       //! cluster PID based on matching
  Int_t             mCluEdepPID;   //!clusterPID based on edep

public: 
  StPmdCluster();           
  StPmdCluster(TArrayI*);   
  ~StPmdCluster();          

  //! member functions

  Int_t           Module() const;
  Int_t           NumofMems() const;
  Float_t           CluEta() const;
  Float_t           CluPhi() const;
  Float_t         CluEdep() const;
  Float_t         CluSigma() const;
  Int_t         CluEdepPID() const;
  Int_t         CluPID() const;

  void            setModule(Int_t);
  void            setNumofMems(Int_t);
  void            setCluEta(Float_t);
  void            setCluPhi(Float_t);
  void            setCluEdep(Float_t);
  void            setCluSigma(Float_t);
  void            setCluEdepPID(Int_t);
  void            setCluPID(Int_t);


  virtual void      Browse(TBrowser *b);
  virtual void      print(ostream *os);

  ClassDef(StPmdCluster,1)// Base class for PMD cluster
};

ostream &operator<<(ostream&, StPmdCluster&); // Printing operator

inline              StPmdCluster::~StPmdCluster(){ /* Nobody */ }

inline   Int_t    StPmdCluster::Module() const {return mModule;}
inline   Int_t    StPmdCluster::NumofMems() const {return mNumofMems;}
inline   Float_t    StPmdCluster::CluEta() const     {return mCluEta;} 
inline   Float_t    StPmdCluster::CluPhi() const    {return mCluPhi;}
inline   Float_t  StPmdCluster::CluEdep() const  {return mCluEdep;}
inline   Float_t  StPmdCluster::CluSigma() const  {return mCluSigma;}
inline   Int_t  StPmdCluster::CluEdepPID() const  {return mCluEdepPID;}
inline   Int_t  StPmdCluster::CluPID() const  {return mCluPID;}

inline   void    StPmdCluster::setModule(Int_t var) {mModule=var;}
inline   void    StPmdCluster::setNumofMems(Int_t var) {mNumofMems=var;}
inline   void    StPmdCluster::setCluEta(Float_t var) {mCluEta=var;}
inline   void    StPmdCluster::setCluPhi(Float_t var) {mCluPhi=var;}
inline   void    StPmdCluster::setCluEdep(Float_t var) {mCluEdep=var;}
inline   void    StPmdCluster::setCluSigma(Float_t var) {mCluSigma=var;}
inline   void    StPmdCluster::setCluEdepPID(Int_t var) {mCluEdepPID=var;}
inline   void    StPmdCluster::setCluPID(Int_t var) {mCluPID=var;}

#endif







