/*******************************************************************
 *
 * $Id: StTofCalibration.cxx,v 1.1 2001/09/28 19:09:39 llope Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Calibration class for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofCalibration.cxx,v $
 * Revision 1.1  2001/09/28 19:09:39  llope
 * first version
 *
 *******************************************************************/
#include <iostream.h>
#include "StTofCalibration.h"
//#include "St_XDFFile.h"
//#include "St_DataSetIter.h"
//#include "ctf/St_ctg_Module.h"

StTofCalibration::~StTofCalibration(){ /* nope */}

StTofCalibration::StTofCalibration(){
  mSlatCalibVec.clear();
  for(int iRow=0; iRow<TOFP_MAX_SLATS; iRow++){
    StructSlatCalib* tempSlatCalib = new StructSlatCalib;
    tempSlatCalib->offset_tdc =  0.;
    tempSlatCalib->cc_tdc     =  25.0e-12;
    tempSlatCalib->ods_tdc    =  0.;
    tempSlatCalib->offset_adc =  0.;
    tempSlatCalib->ods_adc    =  0.;
    tempSlatCalib->cc_adc     =  2.4e-2; // typically this one is nphe_to_adc
    mSlatCalibVec.push_back(*tempSlatCalib);
    delete tempSlatCalib;
  }
}


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

void StTofCalibration::print(){
  cout << "------StTofCalibration:print()----" << endl;
  cout << " CalibrationVector Size = " << mSlatCalibVec.size() << endl;
  for (unsigned int i=0;i<mSlatCalibVec.size();i++){
    cout << " slat: " << i
         << "  tdc: " << mSlatCalibVec[i].offset_tdc  << " " << mSlatCalibVec[i].cc_tdc
         << " " << mSlatCalibVec[i].ods_tdc
         << "  adc: " << mSlatCalibVec[i].offset_adc 
         << " " << mSlatCalibVec[i].ods_adc     << " " << mSlatCalibVec[i].cc_adc
         << endl;
  }
  cout << "----------------------------------" << endl;

}
