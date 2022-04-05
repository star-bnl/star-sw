/*******************************************************************
 *
 * $Id: StTofCalibration.cxx,v 1.5 2003/09/02 17:59:10 perev Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Calibration class for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofCalibration.cxx,v $
 * Revision 1.5  2003/09/02 17:59:10  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.4  2002/01/29 23:33:26  geurts
 * change default tdc calibration factor
 *
 * Revision 1.3  2002/01/22 16:12:42  geurts
 * minor bugfix for Solaris
 *
 * Revision 1.2  2002/01/22 07:11:32  geurts
 * doxygenized
 *
 * Revision 1.1  2001/09/28 19:09:39  llope
 * first version
 *
 *******************************************************************/
//! Time-of-Flight Calibration Utilities
/*! \class StTofCalibration
    \author Frank Geurts

    <p>A package of calibration routines for the STAR Time-of-Flight
    detector. Currently the only method prints the default initialized
    values.<p>

    To do:
    <ul>
    <li> create STAR dBase entries and add members to access those<li>
    <li> move calibration methods from the StTofAnalysisMaker into
         StTofCalibration</li>
    </ul>
*/
#include <Stiostream.h>
#include "StTofCalibration.h"
//#include "St_XDFFile.h"
//#include "St_DataSetIter.h"
//#include "ctf/St_ctg_Module.h"

/// default empty destructor
StTofCalibration::~StTofCalibration(){ /* nope */}

/// default constructor, initializes default calibration values
StTofCalibration::StTofCalibration(){
  mSlatCalibVec.clear();
  for(int iRow=0; iRow<TOFP_MAX_SLATS; iRow++){
    StructSlatCalib* tempSlatCalib = new StructSlatCalib;
    tempSlatCalib->offset_tdc =  0.;
    tempSlatCalib->cc_tdc     =  50.0e-12;
    tempSlatCalib->ods_tdc    =  0.;
    tempSlatCalib->offset_adc =  0.;
    tempSlatCalib->ods_adc    =  0.;
    tempSlatCalib->cc_adc     =  2.4e-2; // typically this one is nphe_to_adc
    mSlatCalibVec.push_back(*tempSlatCalib);
    delete tempSlatCalib;
  }
}


/// initializes calibration from XDF or dBase (not functioning yet)
void StTofCalibration::init(){
  //Char_t *InputXdfFile= "ctg_pars.xdf";
  //St_XDFFile xdf(InputXdfFile);
  //St_DataSet *ctfg = xdf.NextEventGet();
  //St_DataSetIter gime(ctfg);
  //
  //mSlatCalibVec.clear();
  //
  //St_ctg_slat*     stafSlatParam(0);
  //stafSlatParam  = (St_ctg_slat     *) gime("tof_slat");
  //ctg_slat_st *slatCalib = stafSlatParam->GetTable();
  //for(int iRow=0; iRow<stafSlatParam->GetNRows(); iRow++,slatCalib++){
  //  StructSlatCalib* tempSlatCalib = new StructSlatCalib;
  //  tempSlatCalib->offset_tdc =  slatCalib->offset_tdc; // 0.
  //  tempSlatCalib->cc_tdc     =  slatCalib->cc_tdc;     // 25.0e-12
  //  tempSlatCalib->ods_tdc    =  slatCalib->ods_tdc;    // 0.
  //  tempSlatCalib->offset_adc =  slatCalib->offset_adc; // 0.
  //  tempSlatCalib->ods_adc    =  slatCalib->ods_adc;    // 0.
  //  tempSlatCalib->cc_adc     = 0.;  // 2.4e-2 typically this one is nphe_to_adc
  //
  //  mSlatCalibVec.push_back(*tempSlatCalib);
  //  delete tempSlatCalib;
  //}
}

/// dump contents of calibration vector to stdout
void StTofCalibration::print(ostream& os){
  os << "------StTofCalibration:print()----" << endl;
  os << " CalibrationVector Size = " << mSlatCalibVec.size() << endl;
  for (unsigned int i=0;i<mSlatCalibVec.size();i++){
    os  << " slat: " << i
        << "  tdc: " << mSlatCalibVec[i].offset_tdc  << " " << mSlatCalibVec[i].cc_tdc
        << " " << mSlatCalibVec[i].ods_tdc
        << "  adc: " << mSlatCalibVec[i].offset_adc 
        << " " << mSlatCalibVec[i].ods_adc     << " " << mSlatCalibVec[i].cc_adc
        << endl;
  }
  os << "----------------------------------" << endl;

}
