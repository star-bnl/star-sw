/***************************************************************************
 * $Id: StTofMC.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Generate StTofMCSlat from g2t tof-hit.
 *
 *****************************************************************
 * 
 * $Log: StTofMC.cxx,v $
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *
 **************************************************************************/
#include "StTofMC.h"
#include "StTofDatabase.h"
#include "StTofCross.h"
#include "PhysicalConstants.h"
#include "StMessMgr.h"

static const char rcsid[] = "$Id: StTofMC.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $";

ClassImp(StTofMC)
    
StTofMC::StTofMC() 
{/* nopt */}

StTofMC::~StTofMC() {/* nopt */}

// C++ version of cts_detector_response  
StTofMCSlat StTofMC:: slatResponse(int slatId, StTofG2THit* tofHitPtr, 
                          StTofG2TTrack* trackPtr, StTofDatabase* tofDb) {
    StTofSimParam* simParam = new StTofSimParam;
    *simParam = tofDb->tofSimParam();

    StructTofSlat* tofSlat = new StructTofSlat;
    tofSlat = tofDb-> tofSlat(slatId);

    float zMin, zMax;
    float length;
    float maxDistance;
    if(tofSlat->slatEta.eta < 0) {
         zMin = tofSlat->slatEta.z_min;
         zMax = tofSlat->slatEta.z_max;
    }
    else {
         zMin = tofSlat->slatEta.z_max;
         zMax = tofSlat->slatEta.z_min;
    }

    length = (zMax - tofHitPtr->x[2])/tofSlat->slatEta.cosang;
    maxDistance = (zMax - zMin)/ tofSlat->slatEta.cosang;

    if ( length < -1 * simParam->position_tolerance ||
         length - simParam->position_tolerance > maxDistance  ) {
//         gMessMgr->Warning() << "local-Z > length of assigned slat: id = " 
//                                                           << slatId << endm;
    }

// change length instead of avoiding simulation 
    if ( length < -1 * simParam->position_tolerance )
         length = -1 * simParam->position_tolerance; 
    else if ( length - simParam->position_tolerance > maxDistance  ) 
              length = simParam->position_tolerance + maxDistance; 

// work on members of StructTofMCInfo of StTofMCSlat instance MCSlat 
// get number of photoelectrons
    int nPhe;
    if(!(simParam->slat_para)) {
         nPhe = (int) (tofHitPtr->de * slatResponseExp(length, simParam));
    }
    else {
// get distance to closest edges and use maps
         float phi    = atan2 ( tofHitPtr->x[1], tofHitPtr->x[0] ) ;
         if ( phi < 0 ) phi += twopi ;
         float delPhi1 = fabs(tofSlat->slatPhi.phi_min/degree - phi) ;
         if ( delPhi1 > pi ) delPhi1 = twopi - delPhi1 ;
         float delPhi2 = fabs(tofSlat->slatPhi.phi_max/degree-phi) ;
         if ( delPhi2 > pi ) delPhi2 = twopi - delPhi2 ;
         float delEdge = tofDb->tofParam().r * min(delPhi1,delPhi2) ;
         nPhe = (int) (tofHitPtr->de *
           slatResponseTable ( length, delEdge, tofHitPtr->tof, simParam )) ;
    }

    float time = tofHitPtr->tof + simParam->delay * length;
    float resl = simParam->time_res * sqrt(fabs(length));
    if(resl < 50.e-12) resl = 50.e-12;               // pratical lower limit
    float tt = tofHitPtr->tof + gaussian() * resl;

    float ttL = tt + simParam->delay * length + gaussian()*simParam->start_res;

    float de = tofHitPtr->de;
    float pTot2 = 0.0;
    for(int i = 0; i < 3; i++) pTot2 += tofHitPtr->p[i] * tofHitPtr->p[i];
    float pTot;
    if(pTot2 > 0.0) pTot = sqrt(pTot2);
    else            pTot = 0.0; 
    float ds = tofHitPtr->ds;
    float sLength = tofHitPtr->s_track;
    float pmLength= length;
    float tof = tofHitPtr->tof;

// trace back to g2t track and get required MC quantities
    int trkId = tofHitPtr->track_p;
    for(int j = 0; j < trkId - 1; j++) trackPtr++;
    int gId = trackPtr->ge_pid;
    for(int j = 0; j < trkId - 1; j++) trackPtr--;

// set nHits = 1 for now
    int nHits = 1; 

// instance StructMCInfo with obtained quantities
    StructTofMCInfo  MCInfo(trkId, gId, nHits, de, nPhe, pTot, ds, 
                               sLength, pmLength, tof, time, tt, ttL); 
// calculate adc and tdc
    float adcOffset = tofSlat->slatParam.offset_adc +
                       gaussian() * tofSlat->slatParam.ods_adc;
    unsigned short  adc = 
             (unsigned short)((float)nPhe*simParam->nphe_to_adc + adcOffset); 
    float tdcOffset = tofSlat->slatParam.offset_tdc +
                      gaussian() *  tofSlat->slatParam.ods_tdc;
    unsigned short  tdc = 
             (unsigned short)(tt/tofSlat->slatParam.cc_tdc + tdcOffset); 

// instance a StTofMCSalt and set its members
    StTofMCSlat MCSlat;   
    MCSlat.setSlatIndex(slatId);
    MCSlat.setAdc(adc);
    MCSlat.setTdc(tdc);
    MCSlat.setMCInfo(MCInfo);

    return MCSlat;
}

// C++ version of cts_slat_response_table
float StTofMC::slatResponseTable(float z, float dEdge, float tof,
                                           StTofSimParam* simParam) {
    float weight ;

    if (dEdge > simParam->d_grid[simParam->n_d-1]) 
                     dEdge = simParam->d_grid[simParam->n_d-1];

// Find z bin and dEdge bin
    long i,j;

// Use parametrization grid when close to tube
    if (z < simParam->z_grid[simParam->n_z-1]) {
       for (i=1; i<simParam->n_z; i++) 
          if (z  <= simParam->z_grid[i]) break;
       for (j=1; j<simParam->n_d; j++) 
          if (dEdge <= simParam->d_grid[j]) break;

// Interpolate the response from the four corners of the bin.
       float zFrac =(z -simParam->z_grid[i-1])/
             (simParam->z_grid[i]-simParam->z_grid[i-1]);
       float dFrac =(dEdge-simParam->d_grid[j-1])/
             (simParam->d_grid[j]-simParam->d_grid[j-1]);
       long nZ = simParam->n_z;
       float p1 = dFrac*(simParam->slat_response[j*nZ+i]-
                         simParam->slat_response[(j-1)*nZ+i])+
                         simParam->slat_response[(j-1)*nZ+i];
       float p2 = dFrac*(simParam->slat_response[j*nZ+(i-1)]-
                         simParam->slat_response[(j-1)*nZ+(i-1)])+
                         simParam->slat_response[(j-1)*nZ+(i-1)];
       weight = zFrac * (p1-p2) + p2;
    }

// Otherwise just get attenuation
    else if (z < 60.) weight=228.+(60.-z)*0.55;
    else if (z < 80.) weight=213.+(80.-z)*0.75;
    else if (z < 100.) weight=192.+(100.-z)*1.05;
    else if (z < 128.) weight=157.+(128.-z)*1.25;
    else weight=154.+(130.-z)*1.5;

// Normalize result to z=15, d-7
    weight /= 249. ;
    weight = weight * simParam->GeV_2_n_photons
                    * simParam->cath_eff
                    * simParam->cath_surf * simParam->surf_loss;
/*
   get the time of signal integration in ns.
   Assume the gate starts at simParam->gate_t0.
   The width of the gate is taken from simParam->gate_width
*/
    float time = 1.e9 * 
          ( simParam->gate_width -(tof-simParam->gate_t0+z*simParam->delay) );
    long  iTime = (long)time/2;

    float tiPart;
    if (iTime < 0) tiPart = 0.0; 
    else if (time >= simParam->n_time) tiPart = 1.0 ;
    else tiPart = 0.01*simParam->time_response[iTime];

    return weight*tiPart ;
}

// C++ version of cts_slat_response_exp
float StTofMC::slatResponseExp(float length, StTofSimParam* simParam) {

     return  simParam->GeV_2_n_photons * simParam->cath_eff * 
             simParam->cath_surf * simParam->surf_loss * 
             exp(-length / simParam->attlen ); 
}

Float_t  StTofMC::gaussian() 
{
    float        x1, y1, z1, a1, gValue;
    bool         oddCall = true;
    static       float save = 0.1;
    if (oddCall) {
         y1 = 0.0;
         z1 = 0.0;
         while(y1 == 0.0) {
            y1 = float(rand())/RAND_MAX;
         }
         while(z1 == 0.0) {
            z1 = float(rand())/RAND_MAX;
         }
         x1 = twopi * z1;
         a1 = sqrt(-2.0 * log(y1));
         gValue = a1 * sin(x1);
         save   = a1 * cos(x1);
         oddCall = false;
    }
    else {
         gValue = save;
         oddCall = true;
    }
                                 
    return gValue;
}

// methods to manipulate tofMCSlatVector
void
StTofMC::clear()
{
    mMCSlatVec.clear();
}

Bool_t
StTofMC::push_back(StTofMCSlat MCSlat)
{
    mMCSlatVec.push_back(MCSlat);
    return true;
}

StTofMCSlat
StTofMC::front() const
{
    return mMCSlatVec.front();
}

StTofMCSlat
StTofMC::getSlat(size_t index) const
{
    return mMCSlatVec[index];
}

StTofMCSlat
StTofMC::back() const
{
    return mMCSlatVec.back();
}

size_t
StTofMC::size() const
{
    return mMCSlatVec.size();
}

