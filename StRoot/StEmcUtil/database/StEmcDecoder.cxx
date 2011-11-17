#include "StEmcDecoder.h"
#include <time.h>
#include <stdlib.h>
#include <Stiostream.h>
#include <stdio.h>
#ifdef IN_PANITKIN
#include <TFile.h>
#endif
#include "StMessMgr.h"

#include "StEmcMappingDb.h"

ClassImp(StEmcDecoder)

StEmcDecoder::StEmcDecoder(unsigned date, unsigned time, bool TowerMapBug) {
    mapping = StEmcMappingDb::instance();
    
    mTowerMapBug = TowerMapBug; // unused
    
    // tower swap fixes applied only at analysis level
    for(int i=0;i<4800;i++) TowerBugFixIndex[i] = i+1;
    if(date >= 20040101 && date < 20050101) {
        #include "TowerBug2004.txt"
    }
    if(date >= 20050101 && date < 20060101) {
        #include "TowerBug2005.txt"
    }
    for(int i=0;i<4800;i++) {
        int id = i+1;
        if(TowerBugFixIndex[i]!=id) {
            int newId = TowerBugFixIndex[i];
            TowerBugFixIndex[newId-1] = id;
        }
    }
    
    // preshower swap fixes applied only at analysis level
    for(int i=0; i<4800; i++) PreshowerBugFixIndex[i] = i+1;
    if(date >= 20060101 && date < 20071101) {
        #include "PreshowerBug2007.txt"
    }
    for(int i=0; i<4800; i++) {
        int id = i+1;
        if(PreshowerBugFixIndex[i] != id) {
            int newId = PreshowerBugFixIndex[i];
            PreshowerBugFixIndex[newId-1] = id;
        }
    }

    // SMD eta swap fixes applied only at analysis level
    for(int i=0;i<18000;i++) SmdBugFixIndex[i] = i+1;
    if(date >= 20100101 && date < 20110101) {
        #include "SmdBug2010.txt"
    }
}

StEmcDecoder::~StEmcDecoder() { }

void StEmcDecoder::SetDateTime(unsigned int date, unsigned int time) {
    if (mapping) mapping->SetDateTime(date, time);
}

bool StEmcDecoder::GetFixTowerMapBug(void) const {
    return mTowerMapBug;
}

void StEmcDecoder::SetFixTowerMapBug(bool fix) {
    mTowerMapBug = fix; 
}
//--------------------------------------------------------
/*!
GetTowerBugCorrectionShift method - returns the id shift with respect to
the tower index (software) in the original map
\param id_original is the soft_id in the original map
\param id_shift is the shift that should be applied to the id. In this case, id_corrected = Id_original+id_shift
*/
int StEmcDecoder::GetTowerBugCorrectionShift(int id_original,int& id_shift) const
{
    int id_new = TowerBugFixIndex[id_original - 1];
    id_shift = id_new-id_original;
    return 1;
}
//--------------------------------------------------------
/*!
GetPreshowerBugCorrectionShift method - returns the id shift with respect to
the preshower index (software) in the original map
\param id_original is the soft_id in the original map
\param id_shift is the shift that should be applied to the id. In this case, id_corrected = Id_original+id_shift
*/
int StEmcDecoder::GetPreshowerBugCorrectionShift(int id_original,int& id_shift) const
{
    int id_new = PreshowerBugFixIndex[id_original - 1];
    id_shift = id_new-id_original;
    return 1;
}
//--------------------------------------------------------
/*!
GetSmdBugCorrectionShift method - returns the id shift with respect to
the SMD index (software) in the original map
\param id_original is the soft_id in the original map
\param id_shift is the shift that should be applied to the id. In this case, id_corrected = Id_original+id_shift
*/
int StEmcDecoder::GetSmdBugCorrectionShift(int id_original,int& id_shift) const
{
    int id_new = SmdBugFixIndex[id_original - 1];
    id_shift = id_new-id_original;
    return 1;
}
//--------------------------------------------------------
/*!
Copy of StEmcGeom version
\param TowerId is the software id for towers
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
*/
int StEmcDecoder::
GetTowerBin(const int TowerId,int &module,int &eta,int &sub) const {
    if(TowerId<1 || TowerId>4800) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    module = mapping->bemc(TowerId).m;
    eta = mapping->bemc(TowerId).e;
    sub = mapping->bemc(TowerId).s;
    return 1;
}

/*!
\param TDC is the TDC channel number
\param crate is the crate number
*/
int StEmcDecoder::GetTowerCrateFromTDC(int TDC, int& crate) const {
    if(TDC<0 || TDC>29) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    short id = mapping->softIdFromTDC(kBarrelEmcTowerId, TDC, 0);
    crate = mapping->bemc(id).crate;
    return 1;
}

/*!
\param crate is the crate number
\param TDC is the TDC channel number
*/
int StEmcDecoder::GetTowerTDCFromCrate(int crate, int& TDC) const {
    if(crate<1 || crate>30) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    short id = mapping->softIdFromCrate(kBarrelEmcTowerId, crate, 0);
    TDC = mapping->bemc(id).TDC;
    return 1;
}

/*!
\param RDO is the DAQ id
\param TDC is the TDC channel number
*/
int StEmcDecoder::GetTowerTDCFromDaqId(int RDO, int& TDC) const {
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    short id;
    if((id = mapping->softIdFromDaqId(kBarrelEmcTowerId, RDO))) {
        TDC = mapping->bemc(id).TDC;
        return 1; 
    }
    return 0;
}

/*!
\param RDO is the DAQ id
\param crate is the crate number
\param crate_sequency is the position of the tower inside the crate
*/
int StEmcDecoder::
GetTowerCrateFromDaqId(int RDO, int& crate, int& crate_sequency) const {
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    short id;
    if((id = mapping->softIdFromDaqId(kBarrelEmcTowerId, RDO))) {
        crate = mapping->bemc(id).crate;
        crate_sequency = mapping->bemc(id).crateChannel;
        return 1; 
    }
    return 0;
}

/*!
\param RDO is the DAQ id
\param TowerId is the software id for towers
*/
int StEmcDecoder::GetTowerIdFromDaqId(int RDO,int& TowerId) const {
    if (!mapping) return 0;
    short id;
    if((id = mapping->softIdFromDaqId(kBarrelEmcTowerId, RDO))) {
        TowerId = id;
        return 1; 
    }
    return 0;
}

/*!
\param TowerId is the software id for towers
\param RDO is the DAQ id
*/
int StEmcDecoder::GetDaqIdFromTowerId(int TowerId,int& RDO) const {
    if(TowerId<1 || TowerId >4800) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    RDO = mapping->bemc(TowerId).daqID;
    return 1;
}

/*!
\param crate is the crate number
\param crate_sequency is the position of the tower inside the crate
\param TowerId is the software id for towers
*/
int StEmcDecoder::
GetTowerIdFromCrate(int crate,int crate_sequency, int& TowerId) const {
    if(crate < 1 || crate > 30) return 0;
    if(crate_sequency < 0 || crate_sequency > 159) return 0;
    if (!mapping) return 0;
    TowerId = mapping->softIdFromCrate(kBarrelEmcTowerId, crate, crate_sequency);
    return 1;
}

/*!
\param TDC is the crate number
\param tdc_sequency is the position of the tower inside the crate
\param TowerId is the software id for towers
*/
int StEmcDecoder::
GetTowerIdFromTDC(int TDC,int tdc_sequency, int& TowerId) const {
    if(TDC < 0 || TDC > 29) return 0;
    if(tdc_sequency < 0 || tdc_sequency > 159) return 0;
    if (!mapping) return 0;
    TowerId = mapping->softIdFromTDC(kBarrelEmcTowerId, TDC, tdc_sequency);
    return 1;
}

/*!
\param CRATE is the crate number
\param crate_seq is the position of the tower inside the crate (0 <= crate_seq <=159)
\param patchId is the software id for towers
*/
int StEmcDecoder::
GetTriggerPatchFromCrate(int CRATE,int crate_seq, int& patchId) const {
    if(CRATE < 1 || CRATE > 30) return 0;
    if(crate_seq < 0 || crate_seq > 159) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    short id = mapping->softIdFromCrate(kBarrelEmcTowerId, CRATE, crate_seq);
    patchId = mapping->bemc(id).triggerPatch;
    return 1;
}

/*!
\param PATCH is the software id for towers
\param CRATE is the crate number
\param crate_seq is the position of the tower inside the crate (0 <= crate_seq <=159)
*/
int StEmcDecoder::
GetCrateAndSequenceFromTriggerPatch(int PATCH,int& CRATE,int& crate_seq) const {
    if(PATCH<0 || PATCH>299) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    for(int id=1; id<=4800; id++) {
        if(mapping->bemc(id).triggerPatch == PATCH) {
            CRATE = mapping->bemc(id).crate;
            crate_seq = (PATCH%10)*16;
        }
    }
    return 1;
}

/*!
\param jetPatch is the jet patch id (0 <= jetPatch <= 11)
\param jetPatch_seq is the trigger patch position within the jet patch (0 <= jetPatch_seq <= 24)
\param triggerPatch is the trigger patch id (0 <= triggerPatch <=299)
*/
int StEmcDecoder::GetTriggerPatchFromJetPatch(int jetPatch, int jetPatch_seq, 
    int &triggerPatch) const 
{
    if(jetPatch<0 || jetPatch>11) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    for(int id=1; id<=4800; id++) {
        if(mapping->bemc(id).jetPatch == jetPatch) {
            triggerPatch = mapping->bemc(id).triggerPatch;
        }
    }
    // if (jetPatch<0 || jetPatch>=12 || jetPatch_seq<0 || jetPatch_seq>=25)
    //     return 0;
    // triggerPatch = TriggerPatchFromJetPatchAndSeq[(jetPatch * 25) + jetPatch_seq];
    return 1;
}

/*!
\param jetPatch is the jet patch id (0 <= jetPatch <= 11)
\param jetPatch_seq is the trigger patch position within the jet patch (0 <= jetPatch_seq <= 24)
\param triggerPatch is the trigger patch id (0 <= triggerPatch <=299)
*/
int StEmcDecoder::GetJetPatchAndSequenceFromTriggerPatch(int triggerPatch, 
    int &jetPatch, int &jetPatch_seq) const
{
    if (triggerPatch<0 || triggerPatch>299) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    for(int id=1; id<=4800; id++) {
        if(mapping->bemc(id).triggerPatch == triggerPatch) {
            jetPatch = mapping->bemc(id).jetPatch;
        }
    }
    // jetPatch_seq = JetPatchSeqFromTriggerPatch[triggerPatch];
    return 1;
}

int StEmcDecoder::GetDSMFromTriggerPatch(int patchId, int &dsmModule) const {
    if(patchId < 0 || patchId >= 300) return 0;
    dsmModule = patchId/10;
    return 1;
}

int StEmcDecoder::
GetTriggerPatchesFromDSM(int dsmModule, int *triggerPatches) const {
    if(dsmModule < 0 || dsmModule >= 30) return 0;
    for(int i=0; i<10; i++) {
        triggerPatches[i] = dsmModule*10 + i;
    }
    return 1;
}

//--------------------------------------------------------
/*!
\param detector is detector number (3 = SMDE, 4 = SMDP)
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
\param RDO is the SMD fiber number
\param index is the position in the fiber
*/
int StEmcDecoder::GetSmdRDO(int detector, int module, int eta, int sub, 
    int& RDO, int& index) const 
{
    if (!mapping) return 0;
    if(detector == 3) {
	if (!mapping->bsmde()) return 0;
        int id = mapping->softIdFromMES(kBarrelSmdEtaStripId, module, eta, sub);
        RDO = mapping->bsmde(id).rdo;
        index = mapping->bsmde(id).rdoChannel;
        return 1;
    }
    else if(detector == 4) {
	if (!mapping->bsmdp()) return 0;
        int id = mapping->softIdFromMES(kBarrelSmdPhiStripId, module, eta, sub);
        RDO = mapping->bsmdp(id).rdo;
        index = mapping->bsmdp(id).rdoChannel;
        return 1;
    }
    return 0;
}

/*!
\param RDO is the SMD fiber number
\param index is the position in the fiber
\param detector is detector number (3 = SMDE, 4 = SMDP)
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
\param print enables/disables some printout information
*/
int StEmcDecoder::GetSmdCoord(int RDO, int index, int& detector, int& module, 
    int& eta, int& sub, bool print) const 
{
    int wire, A_value;
    return GetSmdCoord(RDO,index,detector,module,eta,sub,wire,A_value,print);
}

/*!
\param RDO is the SMD fiber number
\param index is the position in the fiber
\param detector is detector number (3 = SMDE, 4 = SMDP)
\param module is the module number
\param eta is the eta division for towers
\param sub is the sub division for towers
\param wire is the SMD-wire number
\param A_value is the A_value in the FEE card
\param print enables/disables some printout information
*/
int StEmcDecoder::GetSmdCoord(int RDO, int index, int& det, int& m, int& e, 
    int& s, int& wire, int& A_value, bool print) const 
{
    if (!mapping) return 0;
    short id;
    if((id = mapping->softIdFromRDO(kBarrelSmdEtaStripId, RDO, index))) {
	if (!mapping->bsmde()) return 0;
        det = 3;
        m = mapping->bsmde(id).m;
        e = mapping->bsmde(id).e;
        s = mapping->bsmde(id).s;
        wire = mapping->bsmde(id).wire;
        A_value = mapping->bsmde(id).feeA;
        return 1;
    }
    else if((id = mapping->softIdFromRDO(kBarrelSmdPhiStripId, RDO, index))) {
	if (!mapping->bsmdp()) return 0;
        det = 4;
        m = mapping->bsmdp(id).m;
        e = mapping->bsmdp(id).e;
        s = mapping->bsmdp(id).s;
        wire = mapping->bsmdp(id).wire;
        A_value = mapping->bsmdp(id).feeA;
        return 1;
    }
    return 0;
}

/*!
\param RDO is the PSD fiber number
\param index is the position in the fiber
\param id is the software Id
\param print enables/disables some printout information
 
The Pre Shower and SMD crates are identical and they share almost
the same decoder.
*/
int StEmcDecoder::GetPsdId(int RDO, int index, int& id, bool print) const {
    int wire, A_value, PMT;
    return GetPsdId(RDO, index, id, PMT, wire, A_value, print);
}

/*!
\param RDO is the PSD fiber number
\param index is the position in the fiber
\param id is the software Id
\param PMT is the PMT box number
\param wire is the equivalent mux-wire in the SMD/PSD crate
\param A_value is the A_value in the FEE card
\param print enables/disables some printout information
 
The Pre Shower and SMD crates are identical and they share almost
the same decoder.
*/
int StEmcDecoder::GetPsdId(int RDO,int index, int& softId, int& PMTBox, 
    int& wire, int& A_value, bool print) const
{
    if (!mapping) return 0;
    if (!mapping->bprs()) return 0;
    if((softId = mapping->softIdFromRDO(kBarrelEmcPreShowerId, RDO, index))) {
        PMTBox = mapping->bprs(softId).PMTbox;
        wire = mapping->bprs(softId).wire;
        A_value = mapping->bprs(softId).feeA;
        return 1;
    }
    return 0;
}

/*!
\param id is the software Id
\param RDO is the PSD fiber number
\param index is the position in the fiber
*/
int StEmcDecoder::GetPsdRDO(int id, int& RDO,int& index) const {
    if(id<1 || id>4800) return 0;
    if (!mapping) return 0;
    if (!mapping->bprs()) return 0;
    RDO = mapping->bprs(id).rdo;
    index = mapping->bprs(id).rdoChannel;
    return 1;
}

void StEmcDecoder::PrintTowerMap(ofstream *out) const {
    *out <<"TDC channels connections\n";
    *out <<"-----------------------------------------------------------\n";
    // for(int i=0;i<30;i++)
    //     *out <<"  TDC channel "<<i<<" connected to crate "<<TDC_Crate[i]<<endl;
    *out <<endl;
    *out <<"Tower MAP\n";
    *out <<"-----------------------------------------------------------\n";
    for(int daq =0;daq<4800;daq++)
    {
        int towerId,tdc,crate,position,m,e,s;
        GetTowerIdFromDaqId(daq,towerId);
        GetTowerTDCFromDaqId(daq,tdc);
        GetTowerCrateFromDaqId(daq,crate,position);
        GetTowerBin(towerId,m,e,s);
        *out <<"  daq id = "<<daq<<"  TDC channel = "<<tdc<<"  Crate = "<<crate
        <<"  position in crate = "<<position<<"  software id = "<<towerId
        <<"  m = "<<m<<"  e = "<<e<<"  s = "<<s<<endl;
    }
    *out <<endl;
}

void StEmcDecoder::PrintSmdMap(ofstream *out) const {
    *out <<"Modules connected to SMD crate\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<8;i++)
    {
        *out <<"SMD CRATE number "<<i+1<<endl;
        // for(int j=0;j<15;j++)
        //     *out <<"  channel "<<j<<" is connected to module "
        //     <<SmdModules[i][j]<<endl;
    }
    *out <<"\nSMD MAP\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<8;i++)
    {
        *out <<"SMD CRATE number "<<i+1<<endl;
        for(int index =0;index<4800;index++)
        {
            int det,m,e,s;
            int status = GetSmdCoord(i,index,det,m,e,s);
            *out <<"  RDO = "<<i<<"  index = "<<index;
            if(status == 1)
                *out <<" detector = "<<det<<"  mod = "<<m<<"  eta = "<<e
                <<"  sub = "<<s<<endl;
            else
                *out <<"  dummy connection\n";
        }
    }
    *out <<endl;
}

void StEmcDecoder::PrintPsdMap(ofstream *out) const {
    *out <<"PMT Boxes connected to PSD crate\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<4;i++)
    {
        *out <<"PSD CRATE number "<<i+1<<endl;
        // for(int j=0;j<15;j++)
        //     *out <<"  channel "<<j<<" is connected to PMT Box "
        //     <<PsdModules[i][j]<<endl;
    }
    *out <<"\nPSD MAP\n";
    *out <<"-----------------------------------------------------------\n";

    for(int i=0;i<4;i++)
    {
        *out <<"PSD CRATE number "<<i+1<<endl;
        for(int index =0;index<4800;index++)
        {
            int id;
            int status = GetPsdId(i,index,id);
            *out <<"  RDO = "<<i<<"  index = "<<index;
            if(status == 1)
                *out <<" id = "<<id<<endl;
            else
                *out <<"  dummy connection\n";
        }
    }
    *out <<endl;
}

int StEmcDecoder::GetCrateFromTowerId(int softId, int &crate, int &seq) const {
    if(softId<1 || softId>4800) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    crate = mapping->bemc(softId).crate;
    seq = mapping->bemc(softId).crateChannel;
    return 1;
}

int StEmcDecoder::GetTDCFromTowerId(int softId, int &TDC) const {
    if(softId<1 || softId>4800) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    TDC = mapping->bemc(softId).TDC;
    return 1;
}

int StEmcDecoder::GetTriggerPatchFromTowerId(int softId, int &patchId) const {
    if(softId<1 || softId>4800) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    patchId = mapping->bemc(softId).triggerPatch;
    return 1;
}

int StEmcDecoder::GetJetPatchFromTowerId(int softId, int &jetPatch) const {
    if(softId<1 || softId>4800) return 0;
    if (!mapping) return 0;
    if (!mapping->bemc()) return 0;
    jetPatch = mapping->bemc(softId).jetPatch;
    return 1;
}

int StEmcDecoder::GetTowerIdFromBin(int m, int e, int s, int &softId) const {
    if( (m<1) || (m>120) ) return 0;
    if( (e<1) || (e>20)  ) return 0;
    if( (s<1) || (s>2)   ) return 0;
    if (!mapping) return 0;
    softId = mapping->softIdFromMES(kBarrelEmcTowerId, m, e, s);
    return 1;
}

// $Id: StEmcDecoder.cxx,v 1.5 2010/12/22 22:57:21 stevens4 Exp $
//
// $Log: StEmcDecoder.cxx,v $
// Revision 1.5  2010/12/22 22:57:21  stevens4
// Patch for BSMDE mapping problem in P10ih and P10ij productions (RT #2043)
//
// Revision 1.4  2010/01/28 13:45:06  mattheww
// update from Oleksandr to protect against NULL pointers
//
// Revision 1.3  2009/02/17 22:00:52  mattheww
// fix bug in GetTowerIdFromTDC
//
// Revision 1.2  2009/02/01 17:34:52  kocolosk
// more caching and optimization.
//
// Last StEmcMapping commit was bad and left header, implementation
// inconsistent.  This commit fixes the AutoBuild.
//
// Revision 1.1  2009/01/08 02:16:18  kocolosk
// move StEmcMappingDb/StEmcDecoder to StEmcUtil/database
//
// Revision 2.59  2009/01/02 03:34:33  kocolosk
// use default date==20330101 like St_db_Maker to suppress spurious error msgs
//
// Revision 2.58  2008/12/15 20:25:18  kocolosk
// GetCrateAndSequenceFromTriggerPatch calculated the sequence incorrectly
//
// Revision 2.57  2008/12/05 19:05:32  kocolosk
// new DB-backed implementation of StEmcDecoder
//
// Revision 2.56  2008/11/14 23:25:33  mattheww
// Fixed a lot of BPRS swaps
//
// Revision 2.55  2008/11/07 22:34:16  mattheww
// corrected swap logic for BPRS
//
// Revision 2.54  2008/11/03 19:55:57  mattheww
// fixed a typo causing a mismapping
//
// Revision 2.53  2007/10/10 22:12:35  kocolosk
// SMD module date correction: 20070101, not 20071001
//
// Revision 2.52  2007/10/10 18:52:13  kocolosk
// SMD crate mapping fix for Run 7 and beyond from Oleg
//
// Revision 2.51  2007/10/09 18:02:24  kocolosk
// two extra support functions for TP <=> DSM module mapping
//
// Revision 2.50  2007/09/11 13:30:13  kocolosk
// removed ClassImp that was left in by accident
//
// Revision 2.49  2007/09/11 02:41:37  kocolosk
// added code to fix preshower swaps in 2006 and beyond
//
// Revision 2.48  2007/08/07 19:44:07  perev
// Gene scalers added
//
// Revision 2.47  2007/06/01 17:47:41  jml
// Attempt to fix panitkin plot compile
//
// Revision 2.46  2007/04/09 23:35:13  kocolosk
// 2.45 didn't get it quite right ... thanks to Oleksandr who identified the correct fix for east side JP mapping
//
// Revision 2.45  2007/04/09 13:23:23  kocolosk
// fixed TP -> JP mapping in East barrel
//
// Revision 2.44  2007/04/04 17:35:11  kocolosk
// Added methods GetCrateFromTowerId, GetTDCFromTowerId, GetTDCFromTowerId, GetTriggerPatchFromTowerId, GetJetPatchFromTowerId, and GetTowerIdFromBin.  Also implemented const-correctness and used meaningful argument names in method declarations to improve readability.
//
