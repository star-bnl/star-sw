/***************************************************************************
 *
 * $Id: StEmcTpcFourPMaker.h,v 1.1 2003/04/04 21:36:42 thenry Exp $
 * $Log: StEmcTpcFourPMaker.h,v $
 * Revision 1.1  2003/04/04 21:36:42  thenry
 * Creates lists of Four Vectors by combining data from both the TPC and EMC
 * for use with the StJetMaker
 *
 * Revision 1.0  2003/02/27 21:38:10  thenry
 * Created by Thomas Henry
 *
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a list of Four Momentums from the TPC
 * and EMC corresponding to charged particles and photons, but subtracting 
 * some of the energy deposited in the EMC by the charged particles.
 *
 ***************************************************************************/
#ifndef StEmcTpcFourPMaker_h
#define StEmcTpcFourPMaker_h
#include "StFourPMaker.h"

#define MAXPOINTS 200
#define PHIMODULES 60

class StEmcTpcFourPMaker : public StFourPMaker {
public:
    StEmcTpcFourPMaker(const char* name, StMuDstMaker *pevent);
    virtual Int_t Make();

protected:
    int *trackToPointIndices;   //!
    double *trackEmcPhi;         //!
    bool *trackEmcPhiIsValid;    //!
    double *reducedPointEnergies; //!
    int **pointPhiIndices;        //!
    int *pointPhiIndicesNum;     //!
    double *probPion;            //!
    double *probKaon;            //!
    double *probProton;          //!
    double *probElectron;        //!

    double pi;
    double twoPi;
    double modAngle;
    double pointRadius;
    double PionAveDepRatio;
    double KaonAveDepRatio;
    double ProtonAveDepRatio;
    double ElectronAveDepRatio;

    void probEClobber(int i);

    ClassDef(StEmcTpcFourPMaker,1)
};
#endif
