/***************************************************************************
 * Author: Subhasis Chattopadhyay. 
 ***************************************************************************
 *
 * Description: EMC DB Handling.
 ***************************************************************************/

#ifdef __ROOT__
#ifndef STAR_StEmcHandleDB
#define STAR_StEmcHandleDB

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "TTable.h"  
//#include "tables/St_emcCalSettings_Table.h"
//#include "tables/St_emcCalSummary_Table.h"
#include "tables/St_emcCalibration_Table.h"
//#include "tables/St_emcEqualization_Table.h"
//#include "tables/St_emcMipCalib_Table.h"
//For CC5 compatibility

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif
 
#ifdef ST_NO_TEMPLATE_DEF_ARGS
// Syntax currently required by Solaris compiler
#define StVector(T) vector<T, allocator<T> >
typedef vector<int, allocator<int> > intVector;
typedef vector<Float_t,allocator<Float_t> > FloatVector;
#else
#define StVector(T) vector<T>
typedef vector<int> intVector;
typedef vector<Float_t> FloatVector;
#endif  

class TDataSet;

class StEmcHandleDB {
    
private:
    
protected:
    
public: 
    StEmcHandleDB(TDataSet*);
    virtual       ~StEmcHandleDB();

    //Towers DB
    virtual Int_t  getTowerPeds(Int_t,Int_t,Int_t,Float_t&);  //mod,eta,phi
    //    virtual Int_t  getTowerPeds(Int_t,Float_t&);        //Towerno
    virtual Int_t  getTowerCalibs(Int_t,Int_t,Int_t,Float_t*);
    //   virtual Int_t  getTowerCalibs(Int_t,Float_t&);
    virtual Int_t  getTowerEquals(Int_t,Int_t,Int_t,Float_t&);
    //    virtual Int_t  getTowerEquals(Int_t,Float_t&);

    //SMDE DB
    virtual Int_t  getSmdEPeds(Int_t,Int_t,Float_t&);    //mod,eta
    virtual Int_t  getSmdECalibs(Int_t,Int_t,Float_t&);
    virtual Int_t  getSmdEEquals(Int_t,Int_t,Float_t&);

    //SMDP DB
    virtual Int_t  getSmdPPeds(Int_t,Int_t,Int_t,Float_t&); //mod,eta,phi
    virtual Int_t  getSmdPCalibs(Int_t,Int_t,Int_t,Float_t&);
    virtual Int_t  getSmdPEquals(Int_t,Int_t,Int_t,Float_t&);

    Int_t processDB();
    Int_t processTowerPedDB();
    Int_t processTowerCalibDB();
    Int_t processSmdEDB();
    Int_t processSmdPDB();

protected:
    
private:
    TDataSet* m_calibdbptr;  // Main pointer of calib db

    emcCalibration_st* m_Towercalibdb;  // Calib table for 4 subdetector
    emcCalibration_st* m_Smdecalibdb;  // Calib table for 4 subdetector
    emcCalibration_st* m_Smdpcalibdb;  // Calib table for 4 subdetector


    emcCalibration_st* m_TowerPeddb;   //peds table for 4 subdetectors
    emcCalibration_st* m_Smdepedsdb;   //peds table for 4 subdetectors
    emcCalibration_st* m_Smdppedsdb;   //peds table for 4 subdetectors

    emcCalibration_st* m_Towerequaldb;  //Equalization table for 4 subdetectors
    emcCalibration_st* m_Smdeequaldb;  //Equalization table for 4 subdetectors
    emcCalibration_st* m_Smdpequaldb;  //Equalization table for 4 subdetectors

    FloatVector m_TowerPeds[120][20][2];
    FloatVector m_TowerCalibs[120][20][2];
    FloatVector m_TowerEquals[120][20][2];

    FloatVector m_SmdEPeds[120][150];
    FloatVector m_SmdECalibs[120][150];
    FloatVector m_SmdEEquals[120][150];

    FloatVector m_SmdPPeds[120][10][15];
    FloatVector m_SmdPCalibs[120][10][15];
    FloatVector m_SmdPEquals[120][10][15];

    ClassDef(StEmcHandleDB, 1) 
	};

#endif 
#endif
