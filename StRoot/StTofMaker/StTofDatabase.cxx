/*******************************************************************
 * $Id: StTofDatabase.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 *
 *****************************************************************
 * Description:
 * TOF geometry and parameter database
 *
 *****************************************************************
 *
 * $Log: StTofDatabase.cxx,v $
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *
 *******************************************************************/

// #include "StMessMgr.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "St_XDFFile.h"
#include "StTofDatabase.h"
#include "StThreeVectorD.hh"
#include "tables/St_dst_track_Table.h"
#include "tables/St_tpt_track_Table.h"


#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StTofDatabase.h"

StTofDatabase::StTofDatabase() {}

StTofDatabase::~StTofDatabase() {}

void StTofDatabase::fillDb()
{
    int  iInit = 0;
    mVersion = 1.0;
    static const int nPhiRows5w=300, nPhiRows4w=240;

// read database file of TOF geometry and parameter 

    Char_t *InputXdfFile= "ctg_pars.xdf";
    St_XDFFile xdf(InputXdfFile);
    St_DataSet *ctfg = xdf.NextEventGet();

    assert (ctfg);
//  ctfg->ls("*");
    St_DataSetIter       gime(ctfg);

    StafStTofParam*         stafTofParam(0);
    StafStTofSlatPhi*       stafSlatPhi(0);
    StafStTofSlatEta*       stafSlatEta(0);
    StafStTofSlatParam*     stafSlatParam(0);
    StafStTofSimParam*      stafSimParam(0);

    stafTofParam   = (StafStTofParam     *) gime("tof");
    stafSlatPhi    = (StafStTofSlatPhi   *) gime("tof_slat_phi");
    stafSlatEta    = (StafStTofSlatEta   *) gime("tof_slat_eta");
    stafSlatParam  = (StafStTofSlatParam *) gime("tof_slat");

// check if database is updated 

    Int_t Res_ctg_tof  =  
               ctg(stafTofParam,stafSlatPhi,stafSlatEta,stafSlatParam);
    if (Res_ctg_tof!=kStOK) iInit = kStWarn;

    ctg_geo_st *geo = stafTofParam->GetTable();

// Note: row number starts from 0, while all Ids start from 1 instead of 0
// convert STAF database of ctg_geo to member of StTofDatabase   

    mTofParam = *geo;

// convert STAF database of slats to members (vector) of StTofDatabase    

    int iRow, iEta, iPhi; 

// store eta and phi data base to two vectors first
    ctg_slat_eta_st *eta = stafSlatEta->GetTable();
    for(iRow=0; iRow<stafSlatEta->GetNRows(); iRow++,eta++)  
                                      mTofSlatEtaVec.push_back(*eta);

    ctg_slat_phi_st *phi = stafSlatPhi->GetTable();
//  nRow = stafSlatPhi->GetNRows();    nRow = 300 which is wrong 1/12/01
    for(iRow=0; iRow<780; iRow++,phi++)  mTofSlatPhiVec.push_back(*phi);

// build a database vector of mTofSlatVec.
    ctg_slat_st *slat = stafSlatParam->GetTable();
    for(iRow=0; iRow<stafSlatParam->GetNRows(); iRow++,slat++){
        StructTofSlat* tofSlat = new StructTofSlat;
        tofSlat->slatParam = *slat;
        iEta = slat->i_eta; 
        iPhi = slat->i_phi;                               // 5-slat rows [1]
        if(iEta == 9 || iEta == 12) {                     // 4-slat rows [2]
           if(iPhi <= nPhiRows4w) iPhi = iPhi + nPhiRows5w;
           else iPhi = -1;
        }
        else if(iEta < 9 || iEta > 12) {                  // 4-slat rows [3-10]
           if(iPhi <= nPhiRows4w) iPhi = iPhi + nPhiRows5w + nPhiRows4w; 
           else iPhi = -1;
        }
  

// fill eta and phi information to the slat database. 

        tofSlat->slatEta = mTofSlatEtaVec[iEta-1];
        if(iPhi > 0) {
           tofSlat->slatPhi = mTofSlatPhiVec[iPhi-1];

// add trayId (1 - 60) here

           if(tofSlat->slatEta.ieta == 10 || tofSlat->slatEta.ieta == 11) 
              tofSlat->trayId = (tofSlat->slatPhi.iphi - 1)/5 + 1;    // 5W
           else
              tofSlat->trayId = (tofSlat->slatPhi.iphi - 1)/4 + 1;    // 4W
        }
        else {
          tofSlat->slatPhi.iphi = -1;
          tofSlat->slatPhi.phi = 0.0;
          tofSlat->slatPhi.phi_max = 0.0;
          tofSlat->slatPhi.phi_min = 0.0;
          tofSlat->trayId = -1;
        }

        mTofSlatVec.push_back(*tofSlat);
        delete tofSlat;
    }

// read database file of TOF simulation

    St_XDFFile xdf1;
    xdf1.OpenXDF("cts_pars.xdf");
    St_DataSet *ctfs = xdf1.NextEventGet();

    assert (ctfs);
//  ctfs->ls("*");
    gime.Reset(ctfs);
    stafSimParam          = (StafStTofSimParam    *) gime("cts_tof");
    cts_mpara_st *sim = stafSimParam->GetTable();

// convert STAF database of simulation to StRoot database      

    mTofSimParam = *sim;

}

StructTofSlat* StTofDatabase::tofSlat(Int_t slatId) const
{
    StructTofSlat* thisSlat = new StructTofSlat;
    if(slatId > 0) 
      *thisSlat = mTofSlatVec[slatId-1];   // slatId between 1 - 6000 
    else 
      cout << "slatId = "<<slatId<< "  Wanring: slatId out of range!" << endl;
    return thisSlat;
    
}

StThreeVectorD StTofDatabase::tofSlatNormPoint(Int_t slatId) const
{

// calculate the normal vector <r> to a slat

    double x, y, z, r, cosAng, sinAng, tanAng;

/*
   phi value in STAF database is between 0 to 360 degree instead of
   the StRoot standard of between -PI to PI)
*/

    StructTofSlat* thisSlat = tofSlat(slatId);
    cosAng = thisSlat->slatEta.cosang;
    sinAng = sqrt(1.0 - cosAng*cosAng);                 
    tanAng = fabs(sinAng/cosAng);

    r = (fabs(thisSlat->slatEta.z) + thisSlat->slatEta.r/tanAng) * sinAng;
    x = r * fabs(cosAng) * cos((thisSlat->slatPhi.phi)* degree);
    y = r * fabs(cosAng) * sin((thisSlat->slatPhi.phi)* degree);
    z = r * sinAng * cosAng/fabs(cosAng)*(-1.0);

    StThreeVectorD slatNormPoint = StThreeVectorD(x, y, z);

    return slatNormPoint;

}

StThreeVectorD StTofDatabase::tofPlaneNormPoint(Int_t slatId) const
{

/*
  calculate the normal vector to a slats-plane. 
  tofSlatNormPoint and tofPlaneNormPoint do not always match each other
  because of uncertainties in phi measurments. but the difference is small.   
*/
  

    int iPhi, iEta, centerSlatId;
    int nEtas = tofSlatEtaVec().size();
    StThreeVectorD planeNormPoint(0.0, 0.0, 0.0);  

    StructTofSlat* thisSlat = tofSlat(slatId);
    iEta = thisSlat->slatEta.ieta;

// the normal vector of the center 5W-slat

    if(iEta == 10 || iEta == 11) {
       iPhi = (thisSlat->trayId - 1) * 5 + 3;   
       centerSlatId = (iPhi - 1) * nEtas + iEta; 
       planeNormPoint = planeNormPoint + tofSlatNormPoint(centerSlatId);
    }

// the average over two normal vectors of the center two 4W-slats 

    else {
       iPhi = (thisSlat->trayId - 1) * 4 + 2;   
       for (int j = iPhi; j < iPhi + 2; j++) {
          centerSlatId = (j - 1) * nEtas + iEta; 
          planeNormPoint = planeNormPoint + tofSlatNormPoint(centerSlatId);
       }
       planeNormPoint = planeNormPoint/2.0;
    }

    return planeNormPoint;

}


void StTofDatabase::printGeo(ostream& os) const
{
    os << "**************** StTofDatabase::printGeo() ****************" << endl;

    os << "TOF Detector version:" << mVersion << endl;
    os << "init & id               = " << mTofParam.init << " " 
                                       << mTofParam.detector << endl;
    os << "eta id max & min        = " << mTofParam.i_eta_max << " "  
                                       << mTofParam.i_eta_min << endl;
    os << "phi id max & min        = " << mTofParam.i_phi_max << " "  
                                       << mTofParam.i_phi_min << endl;
    os << "counters/trays/eta(phi) = " << mTofParam.n_counter_eta  << " " 
                                       << mTofParam.n_counter_phi << endl;
    os << "trays in eta & phi      = " << mTofParam.n_tray_eta << " " 
                                       << mTofParam.n_tray_phi << endl;
    os << "slat thickness & width  = " << mTofParam.counter_thickness << " " 
                                       << mTofParam.counter_width << endl;
    os << "mean counter radius     = " << mTofParam.r  << endl;
    os << "tray height & width     = " << mTofParam.tray_height << " "
                                       << mTofParam.tray_width << endl;
    os << "tray length & phi0      = " << mTofParam.tray_length << " "
                                       << mTofParam.tray_phi_zero << endl;
}

void StTofDatabase::printSlat(Int_t slatId, ostream& os) const
{
    os << "************** StTofDatabase::printSlat() ****************" << endl;

    os << "TOF Detector version:" << mVersion << endl;

    os << "---- Slat ----"<< endl;
    os << "slat-tray-eta-phi id=" << " " << slatId << " "
                                  << tofSlat(slatId)->trayId<< " " 
                                  << tofSlat(slatId)->slatParam.i_eta<< " " 
                                  << tofSlat(slatId)->slatParam.i_phi<< endl;
    os << "adc & tdc cal-const= "<< tofSlat(slatId)->slatParam.cc_adc<< " " 
                                  << tofSlat(slatId)->slatParam.cc_tdc<< endl;
    os << "adc & tdc offset   = "<< tofSlat(slatId)->slatParam.offset_adc<< " " 
                                 << tofSlat(slatId)->slatParam.offset_tdc<< endl;
    os << "adc & tdc os-disp  = "<< tofSlat(slatId)->slatParam.ods_adc<< " "  
                                 << tofSlat(slatId)->slatParam.ods_tdc<< endl;
    os << "---- Slat-Eta ---"<< endl;
    os << "eta id & cosang    = "<< tofSlat(slatId)->slatEta.ieta<< " "
                                 << tofSlat(slatId)->slatEta.cosang<< endl;
    os << "eta, its max & min = "<< tofSlat(slatId)->slatEta.eta<< " "
                                 << tofSlat(slatId)->slatEta.eta_max<< " " 
                                 << tofSlat(slatId)->slatEta.eta_min<< endl;
    os << "r, z, z_max, z_min = "<< tofSlat(slatId)->slatEta.r<< " "
                                 << tofSlat(slatId)->slatEta.z<< " " 
                                 << tofSlat(slatId)->slatEta.z_max<< " " 
                                 << tofSlat(slatId)->slatEta.z_min<< endl;
    os << "---- Slat-Phi ---"<< endl;
    os << "id, phi, its max & min  = "<< tofSlat(slatId)->slatPhi.iphi<< " "
                                 << tofSlat(slatId)->slatPhi.phi<< " " 
                                 << tofSlat(slatId)->slatPhi.phi_max << " " 
                                 << tofSlat(slatId)->slatPhi.phi_min<< endl;
}
