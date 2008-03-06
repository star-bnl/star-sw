#ifndef EMC_CALIBRATION_H
#define EMC_CALIBRATION_H

#include <iostream>
//#include <fstream>

using namespace std;

class l3EmcTowerInfo
{
public:
    l3EmcTowerInfo();

    inline float getPhi() { return phi; };
    inline float getEta() { return eta; };
    inline float getEtaMin() { return etamin; };
    inline float getEtaMax() { return etamax; };
    inline float getPed() { return ped; };
    inline float getGain() { return gain; };
    inline int getID() { return id; };
    inline int getDaqID() { return daqId; };

    inline void setPhi(float _phi) { phi = _phi ; };
    inline void setEta(float _eta) { eta = _eta ; };
    inline void setEtaMin(float _etamin) { etamin = _etamin ; };
    inline void setEtaMax(float _etamax) { etamax = _etamax ; };
    inline void setPed(float _ped) {  ped= _ped ; };
    inline void setGain(float _gain) { gain = _gain ; }
    inline void setID(int _id) { id = _id ; } ;
    inline void setDaqID(int _daqid) { daqId = _daqid ; } ;

    inline void set(float _phi, float _eta, float _etamin, float _etamax,
		    float _ped, float _gain, int _id, int _daqID) {
	setPhi(_phi);
	setEta(_eta);
	setEtaMin(_etamin);
	setEtaMax(_etamax);
	setPed(_ped);
	setGain(_gain);
	setID(_id);
	setDaqID(_daqID);
    }

private:
    float phi; 
    float eta;
    float etamin;
    float etamax;
    float ped;
    float gain;
    int id;
    int daqId;
};


// The class l3EmcCalibration translates from DAQ tower IDs to other 
// tower information 

class l3EmcCalibration
{
public:
    l3EmcCalibration(int nTow);
    ~l3EmcCalibration();

    int loadMap(const char *filename);
    int loadTextMap(const char *filename);
    int saveTextMap(const char *filename);
    
    inline l3EmcTowerInfo *getTowerInfo(int ID) {
	return &(tower[ID]);
    }

    inline int daqToId(int daq) { if(daq > nTowers) return 0; else return daq2id[daq]; }

    inline int getNTowers() { return nTowers; }

private: 
    
    int nTowers;
    l3EmcTowerInfo *tower;

    int *daq2id;

    struct colDef_t {

	void set(int _nCols, int _id, int _daqId, int _phi, 
	    int _eta, int _etamin, int _etamax, int _ped, int _gain) {

	    nCols  = _nCols;
	    id     = _id;
	    daqId  = _daqId;
	    phi    = _phi;
	    eta    = _eta;
	    etamin = _etamin;
	    etamax = _etamax;
	    ped    = _ped;
	    gain   = _gain;

	}


	int nCols;

	int id;
	int daqId;
	
	int phi;
	int eta;
	int etamin;
	int etamax;

	int ped;
	int gain;
    };

    int readCalib(ifstream *from, colDef_t colDef);
    
};



#endif
