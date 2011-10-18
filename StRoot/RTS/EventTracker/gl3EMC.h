#ifndef GL3EMC_H
#define GL3EMC_H


#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif

#include "daqFormats.h"
#include "gl3Track.h"
#include "l3EmcCalibration.h"
#ifdef OLD_DAQ_READER
#include <evpReader.hh>
#else /* OLD_DAQ_READER */
#include <DAQ_READER/daqReader.h>
#endif /* OLD_DAQ_READER */

class gl3Event;

class gl3EmcTower {

public:
    inline int   getADC()     { return adc; }
    inline float getEnergy()  { return energy; }
    inline int   getNTracks() { return nTracks; }
    
    inline void setADC(int x)     { 
	adc = x; 

	if(info->getGain() <= 0) 
	    energy = 0.;
	else if(adc==0)
	    energy = 0.;
	else 
	    energy = (adc - info->getPed())*info->getGain();
    }

    inline void setNTracks(int x) { nTracks = x; }
    inline void incrNTracks()     { nTracks++; }

    inline l3EmcTowerInfo *getTowerInfo() { return info; }
    inline void setTowerInfo(l3EmcTowerInfo *_info) { 
        info = _info; 
    }
    
private:
    int   adc;
    float energy;
    int   nTracks;
    
    l3EmcTowerInfo *info;
};

class gl3EMC {
public:
    gl3EMC(l3EmcCalibration *BarrelCalib, l3EmcCalibration *EndcapCalib=NULL);
    ~gl3EMC();


#ifdef OLD_DAQ_READER
    int readFromEvpReader(evpReader *evp, char *mem);
#else /* OLD_DAQ_READER */
    int readFromEvpReader(daqReader *rdr, char *mem);
#endif /* OLD_DAQ_READER */

    int readEMCSECP(EMCSECP* secp);
    int readRawData(L3_P *l3p);
    int matchTracks(gl3Event *event);

    void reset();

    inline float getEtotal()  { return Etotal;};
    inline float getBarrelEnergy() { return Ebarrel;};
    inline float getEndcapEnergy() { return Eendcap;};

    inline int getErrFlags() { return errFlags; }

    inline int getNTowers() { return nTotalTowers; }

    inline gl3EmcTower* getTower(int twr) {
	return &tower[twr];
    }

    inline int getNBarrelTowers() { return nBarrelTowers; }

    inline gl3EmcTower* getBarrelTower(int twr) {
	return &tower[twr];
    }

    inline int getNEndcapTowers() { return nEndcapTowers; }

    inline gl3EmcTower* getEndcapTower(int twr) {
	return &tower[twr+getNBarrelTowers()];
    }


//     inline gl3EmcTower* getTower(gl3Track *track) {

// 	return NULL;
//     }

 private:
    int nBarrelTowers;
    int nEndcapTowers;
    int nTotalTowers;

    gl3EmcTower *tower;
    gl3EmcTower *barrelTower;
    gl3EmcTower *endcapTower;
    
    float Etotal, Ebarrel, Eendcap;

    unsigned int errFlags;

    l3EmcCalibration *barrelCalib;
    l3EmcCalibration *endcapCalib;
};



#endif
