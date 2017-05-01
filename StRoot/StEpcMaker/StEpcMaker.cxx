//
// $Id: StEpcMaker.cxx,v 1.30 2017/04/26 19:46:07 perev Exp $
// $Log: StEpcMaker.cxx,v $
// Revision 1.30  2017/04/26 19:46:07  perev
// Hide m_DataSet
//
// Revision 1.29  2007/01/22 19:13:50  kocolosk
// use STAR logger for all output
//
// Revision 1.28  2005/05/23 12:35:14  suaide
// New Point maker code
//
// Revision 1.27  2004/09/07 14:32:20  suaide
// small changes in the histograms
//
// Revision 1.26  2004/09/03 03:09:45  suaide
// changes in the histograms
//
// Revision 1.25  2004/09/03 00:15:28  jeromel
// Create histo arrays only if histos attribute is set
//
// Revision 1.24  2003/10/02 15:27:54  suaide
// changed some return values to avoid non-necessary printouts
//
// Revision 1.23  2003/09/02 17:58:03  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.22  2003/05/26 13:44:34  suaide
// added setPrint() method
//
// Revision 1.21  2003/04/30 16:08:30  alexst
// Two changes:
// 1. Clear vector of existing barrel points in StEvent. This accounts for the case when one needs to redo points with different parameters.
// 2. Don't require clusters in both SMD planes to proceed with point reconstruction. Only tower clusters are required.
//
// Revision 1.20  2003/01/23 04:03:21  jeromel
// Include fixed
//
// Revision 1.19  2002/02/19 17:42:13  alexst
// Removed a bunch of redundant branching statements and some (but not all) useless output
//
// Revision 1.18  2001/12/03 22:24:28  pavlinov
// tuned for case of no tracks
//
// Revision 1.17  2001/12/01 02:44:49  pavlinov
// Cleanp for events with zero number of tracks
//
// Revision 1.16  2001/11/06 23:35:27  suaide
// fixed bug in the way we get magnetic field
//
// Revision 1.15  2001/11/02 15:32:16  jeromel
// Added return kStOK; which makes Insure happy (function is Int_t and does not return
// a value).
//
// Revision 1.14  2001/11/01 00:15:23  suaide
// Clean up and small modification to stop crashing at Finish()
//
// Revision 1.13  2001/10/24 13:55:05  suaide
// small bugs fixed
//
// Revision 1.12  2001/10/15 01:41:41  pavlinov
// Added Clear method
//
// Revision 1.11  2001/10/03 17:27:37  pavlinov
// clean up for production
//
// Revision 1.10  2001/07/25 15:29:18  subhasis
// check if clusters exist in bemc , bsmde and bsmdp
//
// Revision 1.9  2001/04/25 17:26:13  perev
// HPcorrs
//
// Revision 1.8  2001/04/24 23:06:08  subhasis
// clusters attached to Points, QA hists are made for all category separately
//
// Revision 1.6  2000/08/29 20:40:06  subhasis
//  Modified to accept input from StEvent and writing output to StEvent for Emc
//
// Revision 1.3  2000/05/16 21:48:32  subhasis
//  new checks for events with no clusters
//
// Revision 1.2  2000/05/15 21:53:57  subhasis
// initialversion
//
// Revision 1.1  2000/05/15 21:18:32  subhasis
// initial version
//
// EMC-track Match Maker
//
//
// Authors: Subhasis Chattopadhyay , February 2000.
//

//////////////////////////////////////////////////////////////////////////
//
// StEpcMaker class
//
//////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <TBrowser.h>
#include "StEpcMaker.h"
#include "StPointCollection.h"
#include "StEvent.h"
#include "StEventTypes.h"

ClassImp(StEpcMaker)

//_____________________________________________________________________________
StEpcMaker::StEpcMaker(const char *name):StMaker(name)
{
    mPoint = 0;
    for(int i=0;i<4;i++)
    {
        m_point_energy[i] = 0;   //! //Point Energy spectra
        m_point_eta[i] = 0;      //! //Point Eta spectra
        m_point_phi[i] = 0;      //! //Point Phi spectra
        m_point_sigeta[i] = 0;   //! //Point SigmaEta spectra
        m_point_sigphi[i] = 0;   //! //Point SigmaPhi spectra
        m_point_trmom[i] = 0;    //! //Point TrMom spectra
        m_emc_points[i] = 0;     //! //Emc Point multiplicity
    }
    m_point_flag = 0;
    mFillHisto = kTRUE;
}
//_____________________________________________________________________________
StEpcMaker::~StEpcMaker()
{}
//________________________________________________________________________
Int_t StEpcMaker::Init()
{
    // Making QA histgrams for points
    m_point_flag= new TH1F(" Point_Flag "," Point Flag ",5,0.5,5.5);
    m_point_quality= new TH1F(" Point_Quality "," Point Quality distribution ",32,-0.5,31.5);

    const TString catname[] =
        { "Cat1", "Cat2", "Cat3", "Cat4"
        };

    for (Int_t i=0; i<4; i++)
    {
        TString name_e = catname[i] + "_Point_Energy";
        TString tit_e = catname[i] + " Point Energy";
        m_point_energy[i]= new TH1F(name_e,tit_e,150,0.,15.);

        TString name_eta = catname[i] + "_Point_Eta";
        TString tit_eta = catname[i] + " Point Eta";
        m_point_eta[i]= new TH1F(name_eta,tit_eta,100,-1.,1.);

        TString name_phi = catname[i] + "_Point_Phi";
        TString tit_phi = catname[i] + " Point Phi";
        m_point_phi[i]= new TH1F(name_phi,tit_phi,100,-3.14,3.14);
        if (mFillHisto)
        {
            TString name_sigeta = catname[i] + "_Sigma_Eta";
            TString tit_sigeta = catname[i] + " Sigma Eta";
            m_point_sigeta[i]= new TH1F(name_sigeta,tit_sigeta,100,0.,.2);

            TString name_sigphi = catname[i] + "_Sigma_Phi";
            TString tit_sigphi = catname[i] + " Sigma Phi";
            m_point_sigphi[i]= new TH1F(name_sigphi,tit_sigphi,100,0.,.2);

            TString name_points = catname[i] + "_Points_Multiplicity";
            TString tit_points = catname[i] + " Points Multiplicity";
            m_emc_points[i]= new TH1F(name_points,tit_points,200,0.,200.);

            TString name_mom = catname[i] + "_Track_Momenta";
            TString tit_mom = catname[i] + " Track Momenta ";
            m_point_trmom[i]= new TH1F(name_mom,tit_mom,150,0.,15.);
        }
    }
    return StMaker::Init();
}
//_________________________________________________________________________
Int_t StEpcMaker::Make()
{
    mEvent = (StEvent *) GetInputDS("StEvent");

    if (!mEvent)
    {
        LOG_ERROR << "No StEvent! Can not continue. " << endm;
        return kStOK; // If no event, we're done
    }
    mTheEmcCollection = mEvent->emcCollection();
    if(!mTheEmcCollection)
    {
        LOG_ERROR <<" EPC:: No EmcCollection, Cannot continue**"<<endm;
        return kStOK;
    }

    // ******Creating StPointCollection and calling findPoints
    mPoint = new StPointCollection("point");
//VP    AddData(mPoint);      // for convinience only
    AddData(mPoint);
    if(mPoint->makeEmcPoints(mEvent) != 1)
    {
        return kStOk;
    }
    else {
		LOG_DEBUG << "findEmcPoint == kStOK" << endm;
	}

    MakeHistograms(); // Fill QA histgrams
    LOG_DEBUG << "Epc: ***  Filling StEvent ***" << endm;
    if(fillStEvent() != kStOK)
    {
        LOG_WARN << "StEvent filling is not OK"<<endm;
    }
    return kStOK;
}
//_________________________________________________________________________

void StEpcMaker::MakeHistograms()
{
    Int_t mult[4]={0,0,0,0};
    if(mPoint)
    {
        Int_t nR = mPoint->NPointsReal();

        if(nR>0)
        {
            LOG_DEBUG << "Number of Emc points " << nR << endm;
            TIter next(mPoint->PointsReal());
            StEmcPoint *cl;

            for(Int_t i=0; i<nR; i++)
            {
                cl = (StEmcPoint*)next();
                if(cl)
                {
                    Int_t C = 0;
                    Int_t Q = cl->quality();
                    if( (Q&1) )
                        C = 1; // only tower
                    if( (Q&1) && (Q&4))
                        C = 2; // tower + smd eta
                    if( (Q&1) && (Q&8))
                        C = 3; // tower + smd phi
                    if( (Q&1) && (Q&4) && (Q&8))
                        C = 4; // tower + smd eta + smd phi
                    m_point_flag->Fill(C);
                    m_point_quality->Fill(Q);
                    if(C>0)
                    {
                        mult[C-1]++;
                        if(m_point_energy[C-1])
                            m_point_energy[C-1]->Fill(cl->energy());
                        if(m_point_eta[C-1])
                            m_point_eta[C-1]->Fill(cl->position().pseudoRapidity());
                        if(m_point_phi[C-1])
                            m_point_phi[C-1]->Fill(cl->position().phi());
                        if(m_point_sigeta[C-1])
                            m_point_sigeta[C-1]->Fill(cl->size().x());
                        if(m_point_sigphi[C-1])
                            m_point_sigphi[C-1]->Fill(cl->size().y());
                        StPtrVecTrack& tr = cl->track();
                        for(UInt_t j = 0;j<tr.size();j++)
                        {
                            StTrack* t = tr[j];
                            if(t)
                            {
                                StTrackGeometry *g = t->geometry();
                                if(m_point_trmom[C-1])
                                    m_point_trmom[C-1]->Fill(g->momentum().mag());
                            }
                        }
                    }
                }
            }
        }
    }
    for(Int_t i = 0;i<4;i++)
        if(m_emc_points[i])
            m_emc_points[i]->Fill(mult[i]);
}

Int_t
StEpcMaker::fillStEvent()
{
    LOG_DEBUG <<"Epc::fillStEvent() ***"<<endm;

    if(mPoint)
    {
        Int_t nR = mPoint->NPointsReal();

        if(nR>0)
        {
            LOG_DEBUG << "Number of Emc points " << nR << endm;
            TIter next(mPoint->PointsReal());
            StEmcPoint *cl;

            for(Int_t i=0; i<nR; i++)
            {
                cl = (StEmcPoint*)next();
                mTheEmcCollection->addBarrelPoint(cl);
            }
        }
    }
    else {
		LOG_DEBUG << "There is no BEMC points in this event" <<endm;
	}
    return kStOK;
}
//-------------------------------------------------------
Int_t StEpcMaker::Finish()
{
    return kStOK;
}
void
StEpcMaker::Browse(TBrowser* b)
{
    // Will be see StEmcCollection in browser as separate entity (if unzero) the same as in StEvent
    if(mTheEmcCollection)
        b->Add((TObject*)mTheEmcCollection);
    TDataSet::Browse(b);
}
