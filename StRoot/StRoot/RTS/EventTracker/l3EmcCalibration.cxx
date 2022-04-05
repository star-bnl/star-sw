#include "l3EmcCalibration.h"

#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <fstream>
#include <string>
#include <math.h>
#include <rtsLog.h>
//#include "l3Log.h"

using namespace std;


l3EmcTowerInfo::l3EmcTowerInfo() {
    set(0., 0., 0., 0., 0., 1.0, -1, -1);
}


l3EmcCalibration::l3EmcCalibration(int nTow) {
    nTowers = nTow;

    tower = new l3EmcTowerInfo[nTowers];
    daq2id = new int[nTowers];
}

l3EmcCalibration::~l3EmcCalibration() {
    delete[] tower;
    delete[] daq2id;

}

int l3EmcCalibration::loadMap(const char* filename) 
{

    LOG(ERR, "Binary maps are no longer supported. Where did you find one?",0,0,0,0,0);

    return -1;

}


//#define OLDLOAD
#ifdef OLDLOAD
int l3EmcCalibration::loadTextMap(const char* filename) {
    
    ifstream txtmap(filename);

    string s;

    enum colDesc { col_phi, col_eta, 
		   col_ped, col_threshold, col_gain, 
		   col_id, col_daqId, col_ignore };

    

    txtmap >> s;
    if (s != "EmcCalib") {
      LOG(ERR, "No EMC calibration map in text format in %s\n", filename,0,0,0,0);
	return -1;
    }

    txtmap >> s;
    if (s != "columns:") {
      LOG(ERR,"No EMC calibration map in text format in %s\n", filename,0,0,0,0);
	return -1;
    } 
    
    int nCols;
    txtmap >> nCols;
    //l3Log("EMC calibration map with %d cols\n", nCols);

    colDesc *colTarget = new colDesc[nCols];

    // This should be determined by parsing the header
    colTarget[0] = col_id;
    colTarget[1] = col_daqId;
    colTarget[2] = col_phi;
    colTarget[3] = col_eta;
    colTarget[4] = col_ped;
    colTarget[5] = col_gain;
    colTarget[6] = col_threshold;


    for (int i=0; i<nTowers; i++) {

	float phi=0.;
	float eta=0.;
	float ped=0.;
	float gain=1.0;
	int id=-1;
	int daqId=-1;
	float threshold=0;

	float ignore;

	for (int col=0; col<nCols; col++) {

	    switch (colTarget[col]) {
		
	    case col_phi:   
		txtmap >>  phi ;
		break;

	    case col_eta:   
		txtmap >> eta ;
		break;

	    case col_ped:   
		txtmap >> ped ;
		break;

	    case col_gain:  
		txtmap >> gain ;
		break;

	    case col_threshold: 
		txtmap >> threshold ;
		break;

	    case col_id:    
		txtmap >> id ;
		id--;
		break;

	    case col_daqId: 
		txtmap >> daqId ;
		break;

	    case col_ignore: 
		txtmap >> ignore ;
		break;

	    }
	}
	
//  	cout << "  " << phi << "  " << eta << "  " 
//  	     << ped << "  " << threshold << "  " << gain << "  " 
//  	     << id << "  " << daqId << endl;

	if ( (id < 0) || (id >= 4800) ) {
	  LOG(ERR, "%s contains info for tower %i!!!\n",filename,id,0,0,0);
	    return -1;
	}


	tower[id].set(phi, eta, ped, gain, id, daqId);
	daq2id[daqId] = id;
    }

    return 0;
}
#else 

int l3EmcCalibration::loadTextMap(const char* filename) 
{
    //l3Log("Reading %s", filename);

  //  LOG(NOTE, "Loading calibration map %s",filename,0,0,0,0);

  //#ifdef JEFFDUMB

    string s;
    
    ifstream txtmap(filename);

    string type;
    txtmap >> type;

    txtmap >> s;
    if (s != "columns:") {
      LOG(ERR, "No EMC calibration found in %s\n", filename,0,0,0,0);
	return -1;
    } 
    
    colDef_t colDef; 
    colDef.set(0,-1,-1,-1,-1,-1,-1,-1,-1);

    //int nCols;
    txtmap >> colDef.nCols;

    if (type == "EmcCalib") {

	colDef.set(7,0,1,2,3,-1,-1,4,5);

    } else {
	txtmap >> s;   
        if (s != "format:") return -1;

        for (int i=0; i<colDef.nCols; i++) {
            txtmap >> s;

            if (s=="id")      colDef.id = i;
            if (s=="daq")     colDef.daqId = i;
            if (s=="phi")     colDef.phi = i;
            if (s=="eta")     colDef.eta = i;
            if (s=="etamin")  colDef.etamin = i;
            if (s=="etamax")  colDef.etamax = i;
            if (s=="ped")     colDef.ped = i;
            if (s=="gain")    colDef.gain = i;
        }
	
    }


    int nRead = readCalib(&txtmap, colDef);


    if(nRead == nTowers) 
	return 0;
    else 
	return -1;
    //#endif
    //return 0;
}

#endif

int l3EmcCalibration::readCalib(ifstream *from, colDef_t colDef)
{

    int nTwr = 0;
    //while(1) {
    for (int t=0; t<nTowers; t++)  {
        float phi=0.0, eta=0.0, etamin=-999.0, etamax=-999.0;
        float ped=0.0, gain=1.0;
        int   id=-1, daqId=-1;
        
	string dummy;

	//l3Log("asd %d\n", colDef.nCols);

        for (int i=0; i<colDef.nCols; i++) {
 	    if (i == colDef.id)      *from >> id;     else
 	    if (i == colDef.daqId)   *from >> daqId;  else
 	    if (i == colDef.eta)     *from >> eta;    else
 	    if (i == colDef.etamin)  *from >> etamin; else
 	    if (i == colDef.etamax)  *from >> etamax; else
 	    if (i == colDef.phi)     *from >> phi;    else
 	    if (i == colDef.ped)     *from >> ped;    else
 	    if (i == colDef.gain)    *from >> gain;   else
 	    *from >> dummy;
        }


	if(from->eof()) break;

	// BTOW maps do not have etamin/max, so let's calculate it
	if (nTowers == 4800) {
	    if(etamin == -999.) 
		etamin = floor(eta*20.)/20.;

	    if(etamax == -999.) 
		etamax = ceil(eta*20.)/20.;
	}

	tower[id-1].set(phi, eta, etamin, etamax, ped, gain, id, daqId);
	daq2id[daqId] = id-1;
        nTwr++;
    }
    

    return nTwr;
} 





int l3EmcCalibration::saveTextMap(const char* filename) 
{
    ofstream txtmap(filename);

    txtmap << "EmcCalib" << endl
	   << "columns: 7" << endl;

    for (int i=0; i<nTowers; i++) {
	txtmap << tower[i].getID() << " "
	       << tower[i].getDaqID() << " "
	       << tower[i].getPhi() << " "
	       << tower[i].getEta() << " "
	       << tower[i].getPed() << " "
	       << tower[i].getGain() << " "
	       << 0.0 << endl;
    }

    return 0;
}
