/*!
 * \class StPhmdCluster
 * \author
 */
/********************************************************************
 *
 * $Id: StPhmdCluster.h,v 2.3 2003/09/02 17:58:05 perev Exp $
 *
 * Author: Subhasis Chattopadhyay, Dec 2002
 ********************************************************************
 *
 * Description: Base class for PMD cluster
 *
 ********************************************************************
 *
 * $Log: StPhmdCluster.h,v $
 * Revision 2.3  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.2  2003/04/22 00:08:13  ullrich
 * Removed //! comments
 *
 * Revision 2.1  2002/12/20 22:33:00  ullrich
 * Initial Revision.
 *
 ********************************************************************/
#ifndef STAR_StPhmdCluster
#define STAR_StPhmdCluster

#include <math.h>
#include <Stiostream.h>
#include "StObject.h"
#include "StPhmdHit.h"
#include "StContainers.h"

class StPhmdCluster : public StObject {
public: 
    StPhmdCluster();           
    ~StPhmdCluster();          

    int           module() const;
    int           numberOfCells() const;
    float         eta() const;
    float         phi() const;
    float         energy() const;
    float         sigma() const;
    int           energyPid() const;
    int           pid() const;
    int           mcPid() const;
    
    void          setModule(int);
    void          setNumberOfCells(int);
    void          setEta(float);
    void          setPhi(float);
    void          setEnergy(float);
    void          setSigma(float);
    void          setEnergyPid(int);
    void          setPid(int);
    void          setMcPid(int);
    
    void addHit(StPhmdHit*);
    
    StPtrVecPhmdHit&        hit();
    const StPtrVecPhmdHit&  hit() const;    

private:
    Int_t             mModule;          // supermodule no
    Int_t             mNumberOfCells;   // no. of cells in the cluster
    Float_t           mEta;             // cluster eta
    Float_t           mPhi;             // cluster phi
    Float_t           mEnergy;          // cluster edep
    Float_t           mSigma;           // sigma of the cluster
    Int_t             mPID;             // cluster PID based on matching
    Int_t             mEnergyPID;       // clusterPID based on edep
    Int_t             mMcPID;           // Mc Cluster PID
    StPtrVecPhmdHit   mHits;

    ClassDef(StPhmdCluster,1)
};

ostream &operator<<(ostream&, StPhmdCluster&); // Printing operator


inline   int    StPhmdCluster::module() const    {return mModule;}
inline   int    StPhmdCluster::numberOfCells() const {return mNumberOfCells;}
inline   float  StPhmdCluster::eta() const       {return mEta;} 
inline   float  StPhmdCluster::phi() const       {return mPhi;}
inline   float  StPhmdCluster::energy() const    {return mEnergy;}
inline   float  StPhmdCluster::sigma() const     {return mSigma;}
inline   int    StPhmdCluster::energyPid() const {return mEnergyPID;}
inline   int    StPhmdCluster::pid() const       {return mPID;}
inline   int    StPhmdCluster::mcPid() const     {return mMcPID;}

inline   void   StPhmdCluster::setModule(int var)    {mModule=var;}
inline   void   StPhmdCluster::setNumberOfCells(int var) {mNumberOfCells=var;}
inline   void   StPhmdCluster::setEta(float var)     {mEta=var;}
inline   void   StPhmdCluster::setPhi(float var)     {mPhi=var;}
inline   void   StPhmdCluster::setEnergy(float var)  {mEnergy=var;}
inline   void   StPhmdCluster::setSigma(float var)   {mSigma=var;}
inline   void   StPhmdCluster::setEnergyPid(int var) {mEnergyPID=var;}
inline   void   StPhmdCluster::setPid(int var)       {mPID=var;}
inline   void   StPhmdCluster::setMcPid(int var)     {mMcPID=var;}

ostream& operator<<(ostream &, const StPhmdCluster&);

#endif







