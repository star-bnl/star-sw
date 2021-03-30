// $Id: StFcsFastSimulatorMaker.h,v 1.2 2021/03/30 13:40:09 akio Exp $
//
// $Log: StFcsFastSimulatorMaker.h,v $
// Revision 1.2  2021/03/30 13:40:09  akio
// FCS code after peer review and moved from $CVSROOT/offline/upgrades/akio
//
// Revision 1.8  2021/02/25 21:54:41  akio
// Int_t -> int
//
// Revision 1.7  2021/02/25 19:25:48  akio
// Code modified for STAR code review
//
// Revision 1.6  2021/02/23 16:25:51  akio
// Modification to attend comments from STAR code review (Jason)
//
// Revision 1.5  2020/05/29 18:51:02  akio
// adding EPD g2t reading as PRES
//
// Revision 1.4  2019/10/23 17:15:42  akio
// *** empty log message ***
//
// Revision 1.3  2019/07/22 18:56:41  akio
// Added LeakyHcal option 2 and 3 for 2d light collection efficiency parametrization
//
// Revision 1.2  2019/05/16 16:11:56  akio
// Adding leaky hcal option
//
// Revision 1.1  2018/11/14 16:50:16  akio
// FCS codes in offline/upgrade/akio
//
//
// Declaration of StFcsFastSimulatorMaker, the FCS fast simulator

#ifndef ST_FCS_SIMULATOR_MAKER_H
#define ST_FCS_SIMULATOR_MAKER_H

class StEvent;
class StFcsHit;
class StFcsDb;

#include "StChain/StMaker.h"
#include "StEvent/StEnumerations.h"

/// The FCS fast simulator maker.
/// Populates the FCS hit collection in StEvent with StFcsHits, using Geant hits 
/// from the g2t table as input.
/// This also read from EPD g2t table emulating what happens at actual electronics
/// and DAQ file reading code (StFcsRawHitMaker).
/// Currently no pulse shape & timebin simulation yet, and to be done

class StFcsFastSimulatorMaker : public StMaker {

public:
    
    StFcsFastSimulatorMaker(const char* name = "fcsSim"); 
    virtual ~StFcsFastSimulatorMaker() {}
    int Init();
    void  Clear(Option_t *option="");
    int Make();
    void setDebug(int v=1) {SetDebug(v);}  //!backward compatibility 
 
    /// switching light collection models in xml file 
    ///	 0 = straight Birk				   
    ///	 1 = leaky hcal					   
    ///  2 = xy dep light collection eff 1 side(not leaky)
    ///  3 = xy dep light collection eff 2 side(leaky)    
    void setLeakyHcal(int v=1) {SetAttr("FcsLeakyHcal",v);}

    /// switching z dependent light collection eff in xml file
    ///  0 = straight Birk	
    ///  1 = "noarmal"	
    ///	 2 = "bad"         
    void setHcalZDepEff(int v=1) {SetAttr("FcsHcalZDepEff",v);}
                                                                                                      
private:    
    StFcsDb* mFcsDb;  //! pointer to DB
    void fillStEvent(StEvent* event);   //! Filling StEvent with StFcsHits    
    StFcsHit* mEcalMap[kFcsNorthSouth][kFcsEcalMaxId]; //! table to keep pointers to Ecal hits
    StFcsHit* mHcalMap[kFcsNorthSouth][kFcsHcalMaxId]; //! table to keep pointers to Hcal hits
    StFcsHit* mPresMap[kFcsNorthSouth][kFcsPresMaxId]; //! table to keep pointers to Pres hits

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}  
    ClassDef(StFcsFastSimulatorMaker, 1)
};

#endif  // ST_FCS_SIMULATOR_MAKER_H
