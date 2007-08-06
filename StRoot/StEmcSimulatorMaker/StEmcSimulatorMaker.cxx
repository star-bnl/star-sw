//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEmcSimulatorMaker is class for begin_html <FONT COLOR="RED">EMC Simulation</FONT> end_html dataset
//
//
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <assert.h>
#include <math.h>
#include "TROOT.h"
#include <TRandom.h>
#include <TBrowser.h>
#include <TPad.h>
#include "TList.h"
#include "TObject.h"

#include "StEvent.h"
#include "StEventTypes.h"
#include "StEmcSimulatorMaker.h"
#include "StEmcSimpleSimulator.h"
#include "StEmcPmtSimulator.h"
#include "StPmtSignal.h"
#include "StMcCalorimeterHit.hh"
#include "StMcEmcHitCollection.hh"
#include "StChain.h"

#include "tables/St_g2t_emc_hit_Table.h"
#include "tables/St_ems_hits_Table.h"
#include "tables/St_controlEmcSimulatorMaker_Table.h"

#include "tables/St_emcStatus_Table.h"
#include "tables/St_smdStatus_Table.h"
#include "tables/St_emcCalib_Table.h"
#include "tables/St_smdCalib_Table.h"
#include "tables/St_emcPed_Table.h"
#include "tables/St_smdPed_Table.h"
#include "StEmcUtil/others/emcDetectorName.h"

ClassImp(StEmcSimulatorMaker)


void StEmcSimulatorMaker::clearStEventStaf()
{
    mEmcCollection = 0;
}
StEmcCollection *StEmcSimulatorMaker::getEmcCollection()
{
    return mEmcCollection;
}
St_controlEmcSimulatorMaker *StEmcSimulatorMaker::getControlSimulator()
{
    return controlMaker;
}
St_controlEmcPmtSimulator   *StEmcSimulatorMaker::getControlPmtSimulator()
{
    return pmtSimulator;
}

StEmcSimulatorMaker::StEmcSimulatorMaker(const char *name):StMaker(name)
{
    mBEMC          = 2;  // >0 BEMC
    mEEMC          = 0;  // EEMC  of
    mHistControl   = 1;  // Hist  on
    mCompare       = kFALSE;
    mDB            = 0;
    mEmcCollection = NULL;
    mC1            = NULL;
    m_nhit         = 0;

    geaIn          = 0;
    ems            = 0;
    g2t_emc_hit    = 0;
    g2t_smd_hit    = 0;

    controlMaker   = 0;
    controlTable   = 0;

    pmtSimulator   = 0;
    pmtTable       = 0;

    DB             = 0;
    status         = 0;
    ped            = 0;
    calib          = 0;
    statusEmc      = 0;   // status for BEMC  or BPRS
    statusEmcRec   = 0;
    statusSmd      = 0;   // status for BSMDE or BSMDP
    statusSmdRec   = 0;

    for(int i =0;i<MAXDET;i++)
    {
        mEmcMcHits[i] = NULL;
        mEmcRawHits[i]= NULL;
        mSimulator[i] = NULL;
        mGeom[i] = NULL;
    }
}
void StEmcSimulatorMaker::Clear(const char *)
{
    //if(mEmcCollection)
    //    delete mEmcCollection;
  if (mEmbed) SafeDelete(mEmcCollection);
  StMaker::Clear();
}
StEmcSimulatorMaker::~StEmcSimulatorMaker()
{
  if (mEmbed) SafeDelete(mEmcCollection);
    for (int det=0;det<MAXDET;det++)
        if(mSimulator[det])
            delete mSimulator[det];  //!
}
Int_t StEmcSimulatorMaker::Init()
{
    // checking if this is embedding mode
    // will look for StEmcADCtoEMaker or StEmcMixerMaker

    mEmbed = kFALSE;
    StChain* chain = (StChain*)GetParentChain();
    if(chain)
    {
        TList* l = chain->GetMakeList();
        if(l)
        {
            for(Int_t i = 0;i<l->GetSize();i++)
            {
                TObject* o = l->At(i);
                if(!strcmp(o->ClassName(),"StEmcADCtoEMaker"))
                    mEmbed = kTRUE;
                if(!strcmp(o->ClassName(),"StEmcMixerMaker"))
                    mEmbed = kTRUE;
            }
        }
    }
    LOG_INFO <<"StEmcSimulatorMaker EMBEDDING mode = "<<(Int_t) mEmbed <<endm;

    //
    // Get data from StarDb ( if exist)
    //
    TDataSet *simEmcPar = GetInputDB("emc/simulator");
    pmtTable =NULL;
    if(simEmcPar)
    {
        TDataSetIter local(simEmcPar);
        controlMaker = (St_controlEmcSimulatorMaker*) local("controlEmcSimulatorMaker");
        if(controlMaker)
        {
            controlTable = controlMaker->GetTable();
            mBEMC        = controlTable->bemc;
            mEEMC        = controlTable->eemc;
            mHistControl = controlTable->hist;
            SetDebug(controlTable->debug);

            // Db for calibration
            Int_t detInDb=0;
            for(Int_t i=0; i<4; i++)
            {
                if (controlTable->keyDB[i])
                {
                    detInDb++;
                    if((Int_t)mDB < controlTable->keyDB[i])
                        mDB = controlTable->keyDB[i];
                }
                else
                    controlTable->keyDB[i] = 0;   // push to no DB mode
            }
        }

        pmtSimulator = (St_controlEmcPmtSimulator*) local("Simulator");
        if(pmtSimulator && pmtSimulator->GetNRows() == 4)
            pmtTable = pmtSimulator->GetTable();
    }

    if(mBEMC > 0)
    {
        for(Int_t i=BEMC-1; i<BSMDP; i++)
        {
            if(!mGeom[i])
                mGeom[i] = StEmcGeom::getEmcGeom(i+1);
            if(!mGeom[i])
            {
                LOG_FATAL << "Geometry for detector "<<i+1<<" undefined" << endm;
                assert(0);
            }
            else if(Debug() == 1)
                mGeom[i]->printGeom();

            // Initialise the simulators
            if(mBEMC >= 1)
            {
                if(i<BPRS)
                {
                    StEmcPmtSimulator* pmt;
                    pmt = new StEmcPmtSimulator(i+1);
                    if(pmtTable)
                        pmt->setControl(pmtTable);
                    mSimulator[i] = pmt;
                }
                else
                {
                    StEmcSimpleSimulator* simple;
                    simple = new StEmcSimpleSimulator(i+1);
                    if(pmtTable)
                        simple->setControl(pmtTable);
                    mSimulator[i] = simple;
                }
            }
            if(mHistControl)
                bookHistograms(i);
            if(pmtTable)
                pmtTable++;
        }
    }
    Histograms()->SetName("SimuHist");

    if(mEEMC)
    { /* nothing */
    }	

    saveRunco();

	//set new controlTable flags based on value of mEmbed
	//idea is to get gains and status from DB but not peds if embedding
	//will need to edit to add preshower
	if(mEmbed) {
		controlTable->keyDB[0] = 1;
		controlTable->keyDB[1] = 0;
		controlTable->keyDB[2] = 1;
		controlTable->keyDB[3] = 1;
		LOG_INFO << "StEmcSimulatorMaker controlTable flags have been configured for embedding mode" << endm;
	}		
	
    return StMaker::Init();
}
Int_t StEmcSimulatorMaker::InitRun(Int_t run)
{
    LOG_INFO << "::InitRun() info" << endm;
    LOG_INFO << "=========================================================" << endm;
    for(Int_t i=0; i<4; i++)
    {
        LOG_INFO << Form("Flags for detector %2i",i+1) << endm;
        LOG_INFO << Form("   keyDB       : %i", controlTable->keyDB[i]) << endm;
        LOG_INFO << Form("   pedCutOff   : %f", controlTable->pedCutOff[i]) << endm;
        LOG_INFO << Form("   makeFullDet : %i", controlTable->makeFullDet[i]) << endm;
        LOG_INFO << Form("   calibOffSet : %f", controlTable->calibOffSet[i]) << endm;
        LOG_INFO << Form("   calibSpread : %f", controlTable->calibSpread[i]) << endm;
    }
    LOG_INFO << "========================================================" << endm;


    // making gain fluctuations due to uncertainty in calibration in the
    // detector
    TRandom g;
    for(Int_t i = 0;i<MAXDET;i++)
    {
        for(Int_t j = 0;j<18000;j++)
        {
            mGain[i][j] = 1 + controlTable->calibOffSet[i];
            
            if(controlTable->calibSpread[i]!=0)
                mGain[i][j] += g.Gaus(0,controlTable->calibSpread[i]);
        }
    }
    return kStOk;
}

void StEmcSimulatorMaker::saveRunco()
{
    if(controlMaker)
    {
        St_controlEmcSimulatorMaker* copyControl = new St_controlEmcSimulatorMaker((*controlMaker));
        copyControl->SetName(controlMaker->GetName());
        AddRunco((TDataSet*)copyControl);
        controlMaker = copyControl;              // go away from DB - 31-may-2002 !!
        controlTable = controlMaker->GetTable();
    }
    if(pmtSimulator)
    {
        St_controlEmcPmtSimulator* copyPmt=new St_controlEmcPmtSimulator((*pmtSimulator));
        copyPmt->SetName(pmtSimulator->GetName());
        AddRunco((TDataSet*)copyPmt);
        pmtSimulator = copyPmt;
        pmtTable     = pmtSimulator->GetTable();
    }
}
void StEmcSimulatorMaker::bookHistograms(const Int_t i)
{
    //
    // i - array index (det = i + 1) !!!
    //
    const Char_t* tit[] =
        {"Barrel ","Endcap "
        };

    const Int_t   nx[]  =
        {
            40,40,300,20,12,12,12,12
        };
    const Float_t xl[]  =
        {
            -1.0,-1.0,-1.0,-1.0, 0.5 , 0.5, 0.5, 0.5
        };
    const Float_t xu[]  =
        {
            1.0, 1.0, 1.0, 1.0, 12.5,12.5,12.5,12.5
        };
    const Int_t   ny[]  =
        {
            120, 120, 60, 900, 60, 60, 60, 60
        };
    const Int_t   binEnergySum[4] =
        {
            4000, 1000, 500, 500
        };
    const Float_t energySum[4] =
        {
            100., 10., 50., 50.
        };
    if(!m_nhit)
    {
        m_nhit = new TH2F("EmcNHitsVsDet" ,"Number of hit(log) .vs. Detector #",100,0.0,4.5,8,0.5,8.5);
        m_etot = new TH2F("EmcEtotVsDet" ,"Total energy(log) .vs. Detector #",100,-4.0,4.5,8,0.5,8.5);
    }

    gMessMgr->Info()<<" Book hist for detector " << detname[i].Data() <<endm;

    TString name_h   = detname[i] + "Hits";
    TString name_e   = detname[i] + "Energy";
    TString name_adc = detname[i] + "Adc";
    Int_t ind = (i<BSMDP) ? 0 : 1;
    TString title_h  = tit[ind] + detname[i] + " hits dist.";
    TString title_e  = tit[ind] + detname[i] + " energy dist.";
    TString title_adc= tit[ind] + detname[i] + " ADC dist.";

    Float_t rpiMax = M_PI-0.0001, rpiMin = -M_PI-0.0001; // -pi<=phi<pi
    if(i==2)
    {
        // For SMDE only
        Int_t neta = mGeom[i]->NEta(), iw1, iw2;
        const Float_t* eb = mGeom[i]->Eta();
        TArrayD xb(2*neta+1);
        xb[neta]   = 0.0;
        for(Int_t ik=0; ik<neta; ik++)
        {
            iw1 = neta + 1 + ik;
            iw2 = neta-ik-1;
            Float_t x1 = eb[ik], x2, xw;
            if(ik<neta-1)
            {
                x2 = eb[ik+1];
                xw = (x1+x2)*0.5;
            }
            else
                xw = 0.99;
            xb[iw1] = +xw;
            xb[iw2] = -xw;
            LOG_DEBUG << Form(" iw1 %i %f => iw2 %i %f => eta %f\n", iw1,xb[iw1], iw2,xb[iw2], eb[ik]) << endm;
        }
        // Be carefull with array size !!!
        m_hits[i]   = new TH2F(name_h,title_h, xb.GetSize()-1, xb.GetArray(), ny[i],rpiMin,rpiMax);
        m_energy[i] = new TH2F(name_e,title_e, xb.GetSize()-1, xb.GetArray(), ny[i],rpiMin,rpiMax);
    }
    else
    {
        m_hits[i]   = new TH2F(name_h,title_h, nx[i],xl[i],xu[i], ny[i],rpiMin,rpiMax);
        m_energy[i] = new TH2F(name_e,title_e, nx[i],xl[i],xu[i], ny[i],rpiMin,rpiMax);
    }
    Int_t maxAdc = mGeom[i]->getMaxAdc();
    m_adc[i]     = new TH1F(name_adc,title_adc, maxAdc+1, -0.5, float(maxAdc)+0.5); // ??

    if(mHistControl >= 2)
    {
        TString nameM    = detname[i] + "M";
        TString titModule= tit[ind] + detname[i] + " #Module dist.";
        mhModule[i]      = new TH1F(nameM,titModule, 121, -0.5, 120.5);
        nameM     = detname[i] + "Sub";
        titModule = tit[ind] + detname[i] + " #Sub. dist.";
        mhSub[i]  = new TH1F(nameM,titModule, 15, 0.5, 15.5);
        if(i<4)
        {
            // this is only for checking
            name_e        = detname[i] + "EnergySum";
            title_e       = tit[ind] + detname[i] + " energy dist(sum)";
            mEnergySum[i] = new TH1F(name_e, title_e, binEnergySum[i], 0.0, energySum[i]);
        }
    }

    if(mCompare)
    {
        TString name=detname[i] + "NDif";
        TString tit=detname[i] + " Diff. of hits number";
        mhDiffNumHits[i] = new TH1F(name,tit, 11,-5.5,+5.5);
        name = detname[i] + "DifDe";
        tit  = detname[i] + " Difference of DE";
        mhDiffDe[i] = new TH1F(name,tit, 11,-5.5e-5,+5.5e-5);
    }
}

void StEmcSimulatorMaker::makeHistograms(const Int_t det)
{
    Float_t energysum=0.0, etsum=0.0;
    Float_t E, eta, phi;
    Int_t nhit=0, m,e,s, adc;

    St_emc_hits* emc_hits = mEmcRawHits[det-1];
    Int_t n = emc_hits->GetNRows();

    if(n>0)
    {
        emc_hits_st *hit = emc_hits->GetTable();
        for(Int_t i = 0; i<n; i++)
        {
            m   = (Int_t)hit[i].module;
            e   = (Int_t)hit[i].eta;
            s   = (Int_t)hit[i].sub;
            adc = (Int_t)hit[i].adc;  // For testing only
            E   =        hit[i].energy;

            Int_t ieta=mGeom[det-1]->getEta(m, e, eta);
            Int_t iphi=mGeom[det-1]->getPhi(m, s, phi);
            if(ieta==0 && iphi==0)
            {
                // E could be negative  after subtraction of pedestal - 3-jun-2002
                // If pedType=0, then E must be positive.
                m_hits[det-1]->Fill(eta,phi);
                m_energy[det-1]->Fill(eta,phi,E);
                m_adc[det-1]->Fill(Axis_t(adc));
                nhit      += 1;
                energysum += E;
                etsum     += E/cosh(eta);  // Et = E*sin(theta); sin(theta) = 1./cos(eta)
                if(mHistControl >= 2)
                {
                    if(mhModule[det-1])
                        mhModule[det-1]->Fill(Axis_t(m));
                    if(mhSub[det-1])
                        mhSub[det-1]->Fill(Axis_t(s));
                }
            } 
			else {
				LOG_WARN <<"StEmcSimulatorMaker::makeHistograms=>bad index det "<<det<<" m "<<m<<" e "<<e<<" s "<<s<<endm;
			}
        }
        m_nhit->Fill(log10((Double_t)nhit), (Float_t)det);
        m_etot->Fill(log10((Double_t)energysum), (Float_t)det);
        if(mHistControl >= 2)
            mEnergySum[det-1]->Fill(Double_t(energysum));
    }
}

Int_t StEmcSimulatorMaker::Make()
{
    mEmcCollection = NULL;
    // Changed the order of searching - xdf first.
    static Char_t* typeOfFile[3] = {"xdf", "geant.root", "fz"};
    static Char_t* nameIn[3] = {"event/geant/Event", "geantBranch", "geant"};
    //  Find  Geant  directory with hits
    for(Int_t i=0; i<3; i++)
    {
        geaIn = GetDataSet(nameIn[i]);
        if(geaIn)
        {
            LOG_DEBUG << Form("Type of file -> %s : GEANT directory -> %s\n", typeOfFile[i], nameIn[i]) << endm;
            break;
        }
    }
    if (!geaIn)
    {
        LOG_ERROR <<"Geant Data didn't find in "<< nameIn[0]<<" or "<< nameIn[1]<<endm;
        return kStWarn;
    }

    Int_t retBemc, retEemc;
    retBemc = kStWarn;
    retEemc = kStWarn;
    if(mBEMC)
        retBemc = makeBemc();
    if(mEEMC)
        retEemc = makeEemc();

    fillStEvent();

    return retBemc;   // wait !! and what about retEemc ??
}

Int_t StEmcSimulatorMaker::makeBemc()
{
    //
    // Transition from g2t table to StMcEvent's table style
    //
    for(Int_t i=BEMC-1; i<BSMDP; i++)
    {
        TString nameHits = detname[i] + "McHits";
        mEmcMcHits[i] = new StMcEmcHitCollection(nameHits.Data());
        m_DataSet->Add(mEmcMcHits[i]);
    }

    g2t_emc_hit = (St_g2t_emc_hit *) geaIn->Find("g2t_emc_hit");
    if (g2t_emc_hit )
    {
        if (g2t_emc_hit->GetNRows()>0)
            makeBemcAndBprsMcHits();
        else { 
			LOG_WARN << " makeBemc() => table g2t_emc_hit is empty " << endm;
		}
    }
    else {
        LOG_WARN << " makeBemc() => table g2t_emc_hit isn't found " << endm;
	}

    g2t_smd_hit = (St_g2t_emc_hit *) geaIn->Find("g2t_smd_hit");
    if (g2t_smd_hit)
    {
        if (g2t_smd_hit->GetNRows()>0)
            makeBsmdeAndBsmdpMcHits();
        else {
            LOG_WARN << " makeBemc() => table g2t_smd_hit is empty " << endm;
		}
    }
    else {
        LOG_WARN << " makeBemc() => table g2t_smd_hit isn't found " << endm;
	}

	for(UInt_t i=BEMC-1; i<BSMDP; i++)
            mEmcMcHits[i]->print();

    if(mBEMC >= 2)
        makeAllRawHitsForBemc();

    if(mHistControl)
    {
        makeHistograms(BEMC);
        makeHistograms(BPRS);
        makeHistograms(BSMDE);
        makeHistograms(BSMDP);
    }

    if(mCompare)
        compareOldSimulator();
    return kStOK;
}

void StEmcSimulatorMaker::addBemcAndBprsHit(Int_t module,Int_t eta,Int_t sub,Int_t detector, Float_t de)
{
    StMcCalorimeterHit *emchBemc = NULL, *emchBprs = NULL;

    emchBemc = new StMcCalorimeterHit(module,eta,sub,de); // Don't trace for track

    StMcEmcHitCollection::EAddHit bemcNew = mEmcMcHits[BEMC-1]->addHit(emchBemc);

    if (bemcNew == StMcEmcHitCollection::kNew)
    {
        emchBemc=0;
        if(detector == BPRS)
            emchBprs = new StMcCalorimeterHit(module,eta,sub,de);
    }
    else if(bemcNew == StMcEmcHitCollection::kAdd)
    {
        emchBprs = emchBemc;  emchBemc=0;
    }
    else if(bemcNew == StMcEmcHitCollection::kErr)
    {
        delete emchBemc; emchBemc = 0;
        LOG_WARN <<" Bad hit in Bemc collection " << endm;
    }

    if(detector == BPRS && emchBprs)
    {
        StMcEmcHitCollection::EAddHit bprsNew = mEmcMcHits[BPRS-1]->addHit(emchBprs);
        if(bprsNew != StMcEmcHitCollection::kNew) delete emchBprs;
        emchBprs=0;
    }
    delete emchBemc;
    delete emchBprs;
}
Int_t StEmcSimulatorMaker::makeBemcAndBprsMcHits()
{
    //
    // Decode g2t_emc_hit and fill Mc Hits for BRMC and BPRS.
    // See StMcEventMaker::fillBemc() method.
    //
    Int_t module, eta, sub, detector;
    Float_t de;

    g2t_emc_hit_st *hit = g2t_emc_hit->GetTable();
    Int_t nhits         = g2t_emc_hit->GetNRows();

    Int_t hasHit[2][4800];
    for(Int_t j=0;j<2;j++)
        for(Int_t i=0;i<4800;i++)
            hasHit[j][i] = 0;
    Int_t rid;

    for(Int_t ihit=0; ihit<nhits; ihit++,hit++)
    {
        mGeom[BEMC-1]->getVolIdBemc(hit->volume_id, module,eta,sub,detector);
        de = hit->de;
        if (detector == BEMC || detector == BPRS)
        {
            if(detector == BEMC)
            {
                mGeom[BEMC-1]->getId(module,eta,sub,rid);
                hasHit[0][rid-1] = 1;
            }
            else if(detector == BPRS)
            {
                mGeom[BPRS-1]->getId(module,eta,sub,rid);
                hasHit[1][rid-1] = 1;
            }
            addBemcAndBprsHit(module,eta,sub,detector,de);
        }
    }
    for(Int_t i = 0;i<2;i++)
    {
        if(controlTable->makeFullDet[i])
        {
            for(Int_t j=0;j<4800;j++)
                if(hasHit[i][j]==0)
                {
                    if(i==0)
                    {
                        mGeom[BEMC-1]->getBin(j+1,module,eta,sub);
                        detector = BEMC;
                    }
                    else
                    {
                        mGeom[BPRS-1]->getBin(j+1,module,eta,sub);
                        detector = BPRS;
                    }
                    addBemcAndBprsHit(module,eta,sub,detector,0);
                }
        }
    }
    return kStOk;
}
void StEmcSimulatorMaker::addBsmdeAndBsmdpHit(Int_t module,Int_t eta,Int_t sub,Int_t detector, Float_t de)
{
    StMcCalorimeterHit *emchBsmd;
    emchBsmd = new StMcCalorimeterHit(module,eta,sub,de); // Don't trace for track
    StMcEmcHitCollection::EAddHit bsmdNew = mEmcMcHits[detector-1]->addHit(emchBsmd);

    if(bsmdNew == StMcEmcHitCollection::kAdd)
    {
        delete emchBsmd;
    }
    else if(bsmdNew == StMcEmcHitCollection::kErr)
    {
        delete emchBsmd;
        LOG_WARN <<"StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits=>bad hit in Bsmd collection " << endm;
    }
}
Int_t StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits()
{
    //
    // Decode g2t_emc_hit and fill Mc Hits for BRMC and BPRS => see StMcEventMaker
    // See StMcEventMaker::fillBsmd() method.
    //
    Int_t module, eta, sub, detector;
    Float_t de;

    g2t_emc_hit_st *hit = g2t_smd_hit->GetTable();
    Int_t nhits         = g2t_smd_hit->GetNRows();

    Int_t hasHit[2][18000];
    for(Int_t j=0;j<2;j++)
        for(Int_t i=0;i<18000;i++)
            hasHit[j][i] = 0;
    Int_t rid;

    for(Int_t ihit=0; ihit<nhits; ihit++,hit++)
    {
        mGeom[BSMDE-1]->getVolIdBsmd(hit->volume_id, module,eta,sub,detector);
        de   = hit->de;

        if (detector == BSMDE || detector == BSMDP)
        {
            if(detector == BSMDE)
            {
                mGeom[BSMDE-1]->getId(module,eta,sub,rid);
                hasHit[0][rid-1] = 1;
            }
            else if(detector == BSMDP)
            {
                mGeom[BSMDP-1]->getId(module,eta,sub,rid);
                hasHit[1][rid-1] = 1;
            }
            addBsmdeAndBsmdpHit(module,eta,sub,detector,de);
        }
        else {
			LOG_WARN <<" StEmcSimulatorMaker::makeBsmdeAndBsmdpMcHits=>Bad detector value in Bsmd collection =>" << detector <<endm;
		}
    }
    for(Int_t i = 0;i<2;i++)
    {
        if(controlTable->makeFullDet[i+2])
        {
            for(Int_t j=0;j<18000;j++)
                if(hasHit[i][j]==0)
                {
                    if(i==0)
                    {
                        mGeom[BSMDE-1]->getBin(j+1,module,eta,sub);
                        detector = BSMDE;
                    }
                    else
                    {
                        mGeom[BSMDP-1]->getBin(j+1,module,eta,sub);
                        detector = BSMDP;
                    }
                    addBsmdeAndBsmdpHit(module,eta,sub,detector,0);
                }
        }
    }
    return kStOk;
}
//_____________________________________________________________________________
Int_t StEmcSimulatorMaker::makeAllRawHitsForBemc()
{
    //
    // Transition from deposit energy to adc (and energy)
    //
    UInt_t  m, mf, eta, sub, adc;
    Float_t de, energy;
    Float_t rEta;
    emc_hits_st rawHit;

    emcCalib_st*  calibRec=0;
    smdCalib_st*  calibSmdRec=0;
    emcPed_st*    pedEmcRec=0;
    smdPed_st*    pedSmdRec=0;

    Int_t   pedType=0, cellID=1, cellInd=0, statusALL=0;
    Float_t pedMean=0, pedRMS=0, calCoef=0;

    for(Int_t i=BEMC-1; i<BSMDP; i++)
    {
        status = ped = calib = 0;
        TString dbName = "Calibrations/emc/y3"+detname[i];
        DB = GetInputDB(dbName.Data());

        TString nw = detname[i] + "RawHits"; // Define tables (old style)
        const ULong_t nhits   = mEmcMcHits[i]->numberOfHits();
        mEmcRawHits[i] = new St_emc_hits(nw, nhits);
        m_DataSet->Add(mEmcRawHits[i]);

        rawHit.det = i + 1;
        if(nhits>0)
        {
            if (controlTable->keyDB[i] >=1)
            {
                status   = getStatus(i, DB);
                if(status)
                {
                    if(i<BPRS)
                        statusEmcRec = ((St_emcStatus*)status)->GetTable(0); // one row
                    else
                        statusSmdRec = ((St_smdStatus*)status)->GetTable(0); // one row
                }
                else
                {
                    LOG_FATAL << Form("No status table for detector %i -> %s", i, detname[i].Data()) << endm;
                    assert(0);
                }

                TString tableName = detname[i]+"Calib";
                calib = DB->Find(tableName.Data());

                if(calib)
                {
                    if(i<BPRS)
                        calibRec = ((St_emcCalib*)calib)->GetTable();
                    else
                        calibSmdRec = ((St_smdCalib*)calib)->GetTable();
                    LOG_DEBUG << Form("Calibration table for %s keyDB %i", detname[i].Data(), controlTable->keyDB[i]) << endm; // ??
                    if(Debug()>=2) controlMaker->Print(0,1);
                }
                else
                {
                    LOG_FATAL << Form("No calibration table for detector %i -> %s", i, detname[i].Data()) << endm;
                    assert(0);
                }

                if (controlTable->keyDB[i] >= 2)
                {
                    tableName = detname[i] + "Ped";
                    ped = DB->Find(tableName.Data());
                    if(ped)
                    {
                        if(i<BPRS)
                            pedEmcRec = ((St_emcPed*)ped)->GetTable();
                        else
                            pedSmdRec = ((St_smdPed*)ped)->GetTable();
                    }
                    else
                    {
                        LOG_FATAL << Form("No pedestal table for detector %i -> %s", i, detname[i].Data()) << endm;
                        assert(0);
                    }
                }
                else if(GetEventNumber() <=1 ) {
                    LOG_INFO <<"No pedestal DB for detector "<<i+1<< " keyDB "<< (Int_t)controlTable->keyDB[i]<< endm;
				}
            }

            LOG_DEBUG << Form("Number of modules for detector %i = %d", i, mEmcMcHits[i]->numberOfModules()) << endm;
            for(m=0; m<mEmcMcHits[i]->numberOfModules(); m++)
            {
                mf = m + 1; // m - C style index; mf - Fortran style index !!!!
                const StMcEmcModuleHitCollection* module = mEmcMcHits[i]->module(mf);
                const ULong_t nhm = module->numberOfHits();
                if(nhm>0)
                {
                    //if(mPrint) printf("    Number of hits for modules %i = %d\n", mf, nhm);
                    rawHit.module = mf;
                    const StSPtrVecMcCalorimeterHit hits = module->hits();
                    for(UInt_t ihm=0; ihm<nhm; ihm++)
                    {
                        eta = hits[ihm]->eta();
                        sub = hits[ihm]->sub();
                        de  = hits[ihm]->dE();

                        if(mGeom[i]->getEta(mf, eta, rEta) == 0)  // (m => mf) 15-mar-2001
                        {
                            if(status && (calib || ped)) // use DB
                            {
                                if(mGeom[i]->getId(mf,eta,sub, cellID) == 0)
                                {
                                    cellInd  = cellID - 1; // C++ index
                                    if(i<BPRS)
                                        statusALL = Int_t(statusEmcRec->Status[cellInd]);
                                    else
                                        statusALL = Int_t(statusSmdRec->Status[cellInd]);

                                    if(statusALL!=1)
                                        continue;               // bad common status - skip this hit

                                    if(i<BPRS) {
                                        calCoef = calibRec[0].AdcToE[cellInd][1]; // AdcToE[0] - discard now
                                    	LOG_DEBUG << Form("det %i cellID %4i m %3i eta %3i sub %2i AdcToE[0][0] %f AdcToE[1][0] %f", i+1, cellID, mf, eta, sub, calibRec->AdcToE[0][0], calibRec->AdcToE[1][0]) << endm; //VP
				    }
                                    else
                                        calCoef = calibSmdRec[0].AdcToE[cellInd][1];

                                    if(calCoef < 1.e-7) {
                                        LOG_DEBUG << Form("det %i cellID %4i m %3i eta %3i sub %2i c %f", i+1,cellID,mf,eta,sub,calCoef) << endm;
									}

                                    if(ped)
                                    {
                                        pedType = 1;      // gauss dustribution
                                        if(i<BPRS)
                                        {
                                            pedMean   = pedEmcRec[0].AdcPedestal[cellInd]/100.;
                                            pedRMS    = pedEmcRec[0].AdcPedestalRMS[cellInd]/100.;
                                        }
                                        else
                                        {
                                            pedMean   = pedSmdRec[0].AdcPedestal[cellInd][0]/100.;
                                            pedRMS    = pedSmdRec[0].AdcPedestalRMS[cellInd][0]/100.;
                                        }
                                    }
                                    else
                                        pedType = 0; // no pedestal
                                    mSimulator[i]->setParameters(calCoef, pedType, pedMean, pedRMS,mGain[i][cellInd]);
                                }
                                else
                                    continue; // skip this hit
                            } else {
                                LOG_DEBUG <<"StEmcSimulatorMaker::makeAllRawHitsForBemc() => not using DB for det "<<i+1<<" status "<<status<<" calib "<<calib<<" ped "<<ped<<endm;
							}

                            adc    = mSimulator[i]->getAdc((Double_t)de, (Double_t)rEta);
                            if(controlTable->pedCutOff[i]>0)
                                if((adc-pedMean)<controlTable->pedCutOff[i]*pedRMS)
                                    adc = 0;
                            if(adc>0) // Zero suppression
                            {
                                energy = mSimulator[i]->getEnergy();
                                rawHit.eta    = eta;
                                rawHit.sub    = sub;
                                rawHit.adc    = adc;
                                rawHit.energy = energy;
                                mEmcRawHits[i]->AddAt(&rawHit);
                            }
                        } else {
                            LOG_WARN <<"StEmcSimulatorMaker::makeAllRawHitsForBemc() Bad m "<<m<<" or eta "<<eta <<endm;
						}
                    }
                }

            }
        }
        else {
            LOG_WARN <<"StEmcSimulatorMaker -> no hits for detector " << i + 1 << endm;
		}
        ;
    }
    return kStOk;
}

Int_t StEmcSimulatorMaker::makeEemc()
{
    return kStOk;
}

Int_t StEmcSimulatorMaker::fillStEvent()
{
    mEmcCollection = NULL;
    if(!mEmbed)
    {
        StEvent* event = (StEvent*)GetInputDS("StEvent");
        if(!event)
        {
            event = new StEvent();
            AddData(event);
        }
        mEmcCollection = event->emcCollection();
        if(!mEmcCollection)
        {
            mEmcCollection = new StEmcCollection();
            event->setEmcCollection(mEmcCollection);
        }
    }
    else
    {
        mEmcCollection = new StEmcCollection();
    }
    for(Int_t i=0; i<MAXDET; i++)
    {
        StDetectorId id = (StDetectorId)(i+kBarrelEmcTowerId);
        St_emc_hits  *table = mEmcRawHits[i];
        if(table)
        {
            StEmcDetector* detector = new StEmcDetector(id, 120);
            mEmcCollection->setDetector(detector);
            emc_hits_st *t = table->GetTable();
            for(Int_t j=0; j<table->GetNRows(); j++)
                if(t[j].adc>0)
                {
                    StEmcRawHit* hit = new StEmcRawHit(id,t[j].module, t[j].eta, t[j].sub,t[j].adc, t[j].energy);
                    hit->setCalibrationType(0);
                    detector->addHit(hit);
                }
        }
    }
    return kStOK;
}
void StEmcSimulatorMaker::Browse(TBrowser* b)
{
    //  Will be see StEmcCollection in browser as separate entity (if unzero)
    if(mEmcCollection)
        b->Add((TObject*)mEmcCollection);
    TDataSet::Browse(b);
}

TDataSet* StEmcSimulatorMaker::getStatus(const Int_t ind, TDataSet* dataset)
{ // service routine
    if(!dataset || ind<0 || ind>3)
        return 0;
    TString tableName = detname[ind]+"Status";
    TDataSet* s = dataset->Find(tableName.Data());
    return s;
}

void StEmcSimulatorMaker::printmBEMC()
{
    if(!mBEMC) {
        LOG_INFO <<" BEMC out of CHAIN  mBEMC="<<mBEMC<<endm;
	}
    else {
        LOG_INFO <<" BEMC     in CHAIN  mBEMC="<<mBEMC<<endm;
	}
}

void StEmcSimulatorMaker::compareOldSimulator()
{
    //
    // 21-mar-2001 for comparing "new" and "old" simulator (deposit energy)
    //
    ems = GetDataSet("emc_raw/.data");
    St_ems_hits *bemc;
    ems_hits_st *tab;
    StMcEmcHitCollection* bemcM;
    StMcEmcModuleHitCollection* module;
    StMcCalorimeterHit* hMC;

    Int_t nbemc[4]={0,0,0,0};
    Int_t det, m, e, s;
    Float_t de;

    bemc = (St_ems_hits*)ems->FindByName("ems_hits_bemc"); // bemc + bprs
    for(Int_t ibr=1; ibr<=2; ibr++)
    {
        tab = bemc->GetTable();
        for(Int_t nh=0; nh<bemc->GetNRows(); nh++)
        {
            det = tab[nh].det;
            m   = tab[nh].module;
            e   = tab[nh].eta;
            s   = tab[nh].sub;
            de  = tab[nh].energy;

            bemcM  = getEmcMcHits(det);
            module = bemcM->module(m);
            nbemc[det-1]++;

            StSPtrVecMcCalorimeterHit& hits = module->hits();
            for(Int_t mh=0; mh<(Int_t)hits.size(); mh++)
            {
                hMC = hits[mh];
                if(m==hMC->module() && e==hMC->eta() && s==hMC->sub())
                {
                    mhDiffDe[det-1]->Fill(de-hMC->dE());
                    goto ENDCYCLE;
                }
            }
            LOG_WARN << "Did not find New hit for OLD !!!" << endm;
ENDCYCLE:
            continue;
        }
        bemc = (St_ems_hits*)ems->FindByName("ems_hits_bsmd"); // shower max
    }
    for(Int_t i=0; i<4; i++)
    {
        Int_t det=i+1;
        Int_t nOld = nbemc[i];
        Int_t nNew = getEmcMcHits(det)->numberOfHits();
        mhDiffNumHits[i]->Fill(float(nOld-nNew));
    }
}

void StEmcSimulatorMaker::pictureAllDetectors(Int_t print)
{
    //
    // 22-mar-2001 for convinience
    //
    if(!mC1)
        mC1 = new TCanvas("mC1","Picture for all detectors",0,25,600,800);
    else
        mC1->SetTitle("Picture for all detectors");

    mC1->Clear();
    mC1->Divide(1,2);

    mC1->cd(1);
    m_nhit->SetLineWidth(4);
    m_nhit->Draw();
    mC1->cd(2);
    m_etot->SetLineWidth(4);
    m_etot->Draw();
    mC1->Update();
    if(print)
        mC1->Print("ps/newSim/allDetectors.ps");
}

void StEmcSimulatorMaker::pictureForDetector(Int_t det, Int_t logy, Int_t print)
{
    if(!mC1)
        mC1 = new TCanvas("mC1","Picture for detector",0,25,600,800);
    else
        mC1->SetTitle("Picture for detector");

    mC1->Clear();
    mC1->Divide(1,3);

    Int_t i = det-1;
    i = (i<0)?0:((i>3)?3:i);

    mC1->cd(1);
    m_hits[i]->SetLineWidth(4);
    m_hits[i]->Draw();
    mC1->cd(2);
    m_energy[i]->SetLineWidth(4);
    m_energy[i]->Draw();
    mC1->cd(3);
    m_adc[i]->SetLineWidth(4);
    gPad->SetLogy(logy);
    m_adc[i]->Draw();
    mC1->Update();
    if(print)
    {
        TString name("ps/newSim/Det");
        name += detname[i] + ".ps";
        mC1->Print(name.Data());
    }
}

void StEmcSimulatorMaker::pictureCompareDe(Int_t print)
{
    //
    // 22-mar-2001 for comparing "new" and "old" simulator (deposit energy)
    //
    if(mCompare)
    {
        if(!mC1)
            mC1 = new TCanvas("mC1","DE comparing for OLD and NEW Simulator",0,25,600,800);
        else
            mC1->SetTitle("DE comparing for OLD and NEW Simulator");

        mC1->Clear();
        mC1->Divide(2,4);
        for(Int_t i=0; i<4; i++)
        {
            mC1->cd(2*i+1);
            mhDiffNumHits[i]->SetLineWidth(4);
            mhDiffNumHits[i]->Draw();
            mC1->cd(2*i+2);
            mhDiffDe[i]->SetLineWidth(4);
            mhDiffDe[i]->Draw();
        }
        mC1->Update();
        if(print)
            mC1->Print("ps/newSim/CompareOldNewDe.ps");
    }
    else {
        LOG_INFO << "Picture unavailable : mCompare is zero" << endm;
	}
}

void StEmcSimulatorMaker::printSimulator(Int_t det)
{
    Int_t ind1=det-1, ind2 = det-1;
    if(det==0)
    {
        ind1 = 0;
        ind2 = 3;
    }
    if(ind1<0)
        ind1 = 0;
    if(ind2>3)
        ind2 = 3;
    for(Int_t ind=ind1; ind<=ind2; ind++)
    {
        Int_t det = ind + 1;
        if(mSimulator[ind])
            mSimulator[ind]->print();
        else {
			LOG_INFO << Form("Simulator for detector %1i undefined", det) << endm;
		}
    }
}

void StEmcSimulatorMaker::printStatusTable(Int_t det, Int_t hist)
{
	TString printout = "";
    Int_t ind=det-1, detID, m, e, s, cellIn=0, cellOut=0;
    Float_t eta, phi;
    if(ind<0)
        ind = 0;
    if(ind>3)
        ind = 3;
    TH2F* hIdStatus=0;

    TString dbName = "Calibrations/emc/y3"+detname[ind];
    DB = GetInputDB(dbName.Data());
    status   = getStatus(ind, DB);
    if(hist)
    {
        hIdStatus = (TH2F*)m_hits[ind]->Clone();
        TString name("hStatus");
        name += detname[ind];
        hIdStatus->SetName(name.Data());
        name = "status for detector " + detname[ind];
        hIdStatus->SetTitle(name.Data());
        hIdStatus->Reset();
    }
    if(ind<2)
    {
        statusEmc    = (St_emcStatus*)status;
        statusEmcRec = statusEmc->GetTable();
        for(Int_t i=0; i<4800; i++)
        {
            detID = i + 1;
            if(statusEmcRec[0].Status[i]==1)
            {
                cellIn++;
                mGeom[ind]->getBin(detID, m, e, s);
                printout += Form(" ID %4.4i m %2.2i e %2.2i s %1i    ", detID, m, e, s);
                if(hist)
                {
                    mGeom[ind]->getEtaPhi(detID, eta, phi);
                    hIdStatus->Fill(eta, phi, 1.);
                }
                if(cellIn%3 == 0)
                    printout += Form("\n");
            }
            else
            {
                cellOut++;
				printout += Form(" ID %4.4i -> status %i\n", detID, Int_t(statusEmcRec[0].Status[i]));
                if(cellIn%4 == 0)
					printout += Form("\n");
            }
        }
    }
    else
    {
        for(Int_t i=0; i<18000; i++)
        {
            detID = i + 1;
            statusSmd    = (St_smdStatus*)status;
            statusSmdRec = statusSmd->GetTable();
            if(statusSmdRec[0].Status[i]==1)
            {
                cellIn++;
                mGeom[ind]->getBin(detID, m, e, s);
                if(hist)
                {
                    mGeom[ind]->getEtaPhi(detID, eta, phi);
                    hIdStatus->Fill(eta, phi, 1.);
                }
                printout += Form(" ID %4.4i m %2.2i e %3.3i s %2.2i  ", detID, m, e, s);
                if(cellIn%4 == 0)
                    printout += Form("\n");
            }
            else
            {
                cellOut++;
                printout += Form(" ID %4.4i -> status %i ", detID, Int_t(statusEmcRec[0].Status[i]));
                if(cellIn%4 == 0)
					printout += Form("\n");
            }
        }
    }
	LOG_INFO << printout << endm;
    LOG_INFO << Form("Table name %s : DB: date %i : time %i",
           status->GetName(), controlTable->dateDB, controlTable->timeDB) << endm;
    LOG_INFO << Form("Detector %i : cells(in) %i : cells(out) %i -> sum %i", det, cellIn, cellOut, cellIn+cellOut) << endm;
	if(Debug()>=2) controlMaker->Print(0,1);
    if(hist)
        hIdStatus->Draw();
}

//////////////////////////////////////////////////////////////////////////
// $Id: StEmcSimulatorMaker.cxx,v 1.44 2007/08/06 22:55:56 kocolosk Exp $
// $Log: StEmcSimulatorMaker.cxx,v $
// Revision 1.44  2007/08/06 22:55:56  kocolosk
// fixed a logic error in logging that was causing segfaults (RT #1012)
//
// Revision 1.43  2007/07/13 13:44:20  fisyak
// Delete mEmcCollection if set mEmbed
//
// Revision 1.42  2007/04/05 19:04:33  kocolosk
// fix AutoBuild warning
//
// Revision 1.41  2007/03/22 22:48:28  perev
// Small old bug fix, thanx to Oleksandr
//
// Revision 1.40  2007/03/22 21:51:36  perev
// Leak of StMcCalorimeterHit fix
//
// Revision 1.39  2007/01/23 20:38:59  kocolosk
// logger update
//
// Revision 1.38  2007/01/23 20:36:25  kocolosk
// oops ... keyDb should have been keyDB in rev.1.37
//
// Revision 1.37  2007/01/23 20:14:21  kocolosk
// added code in Init() toautomatically set embedding mode controlTable flags if StEmcADCtoEMaker and/or StEmcMixerMaker.  Users do not need to do this in their own macros any more.
//
// Revision 1.36  2007/01/23 19:44:24  kocolosk
// few additional logger fixes
//
// Revision 1.35  2007/01/22 19:13:40  kocolosk
// use STAR logger for all output
//
// Revision 1.34  2006/09/20 13:44:25  kocolosk
// fix autobuild warnings
//
// Revision 1.33  2006/02/16 16:11:41  suaide
// small modification in the way the calibration spread/offset is created
//
// Revision 1.32  2006/01/24 16:31:47  suaide
// disabled printout
//
// Revision 1.31  2005/05/13 15:49:36  suaide
// set correct StEmcRawHit::calibrationType() for simulated hits
//
// Revision 1.30  2005/03/21 21:36:39  suaide
// fixed problem with chain
//
// Revision 1.29  2005/01/07 11:31:20  suaide
// small bug fixed
//
// Revision 1.28  2004/08/09 19:43:28  suaide
// moved global variables to private members and
// made small modifications to run in embedding mode
//
// Revision 1.27  2004/08/06 13:24:48  suaide
// New features added and fixed some bugs in the database
//
// Revision 1.26  2004/04/09 21:33:53  perev
// Cleanup. destructor of maker more deleting
//
// Revision 1.25  2004/04/08 21:35:12  perev
// Leak off
//
// Revision 1.24  2003/10/01 00:43:16  pavlinov
// Change searching order for Geant hits
//
// Revision 1.23  2003/09/30 01:28:49  jeromel
// Undo correction until logic reshape
//
// Revision 1.22  2003/09/28 03:06:01  jeromel
// restored leak_assign (logic needs to be modified to get rid of it)
//
// Revision 1.21  2003/09/28 01:57:55  jeromel
// LEAK_SCOPE and LEAK_ASSIGN removed
//
// Revision 1.20  2003/09/23 15:19:52  suaide
// fixed bugs and modifications for embedding
//
// Revision 1.18  2003/09/02 17:58:00  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.17  2003/04/30 20:36:47  perev
// Warnings cleanup. Modified lines marked VP
//
// Revision 1.16  2003/01/23 03:09:02  jeromel
// Include modif
//
// Revision 1.15  2003/01/17 21:21:28  suaide
// small bug fixed to compile on Solaris
//
// Revision 1.14  2003/01/17 00:44:20  suaide
// Added new EMC database scheme
//
// Revision 1.13  2002/09/17 18:37:01  pavlinov
// mDbMaker was zero
//
// Revision 1.12  2002/09/16 22:14:50  pavlinov
// No DB for EMC before 24-09-2001
//
// Revision 1.11  2002/09/10 16:51:32  pavlinov
// Discard line with mDbMaker->SetDateTime
//
// Revision 1.10  2002/06/04 16:09:36  pavlinov
// added option with DB(pedestal ans calibration  coefficients
//
// Revision 1.9  2002/06/03 23:35:10  pavlinov
// Last correction without DB for ped and calib. coeff.
//
// Revision 1.8  2002/05/30 17:35:06  pavlinov
// changed the way of searching of GEANT data
//
// Revision 1.7  2001/09/22 00:29:42  pavlinov
// No public constructor for StEmcGeom
//
// Revision 1.6  2001/05/14 01:21:45  pavlinov
// In method StMcEmcHitCollection::module(m) m is module number, not index
//
// Revision 1.5  2001/03/23 19:02:51  pavlinov
// Get pointer to chain via list of browsables
//
// Revision 1.4  2001/03/22 22:04:38  pavlinov
// Clean up for mdc4
//
// Revision 1.3  2001/03/15 17:21:32  pavlinov
// Fixed error for module=1
//
// Revision 1.2  2001/02/02 23:59:59  pavlinov
// New function Browse() and cleanup for new version of BFC
//
// Revision 1.1  2000/10/23 22:53:14  pavlinov
// First working C++ version
//
//////////////////////////////////////////////////////////////////////////
