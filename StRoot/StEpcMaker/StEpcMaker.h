//
// $Id: StEpcMaker.h,v 1.13 2014/08/06 11:43:08 jeromel Exp $
//
// $Log: StEpcMaker.h,v $
// Revision 1.13  2014/08/06 11:43:08  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.12  2007/01/22 19:13:50  kocolosk
// use STAR logger for all output
//
// Revision 1.11  2005/05/23 12:35:14  suaide
// New Point maker code
//
// Revision 1.10  2004/09/13 13:59:56  suaide
// small change
//
// Revision 1.9  2004/09/03 03:09:45  suaide
// changes in the histograms
//
// Revision 1.8  2003/09/10 19:47:13  perev
// ansi corrs
//
// Revision 1.7  2003/05/26 13:44:34  suaide
// added setPrint() method
//
// Revision 1.6  2001/12/01 02:44:50  pavlinov
// Cleanp for events with zero number of tracks
//
// Revision 1.5  2001/11/06 23:35:27  suaide
// fixed bug in the way we get magnetic field
//
// Revision 1.4  2001/10/15 01:41:41  pavlinov
// Added Clear method
//
// Revision 1.3  2001/04/24 22:50:37  subhasis
// clusters attached to Points, QA hists are made for all category separately
//
// Revision 1.2  2000/08/29 20:12:44  subhasis
// Modified to accept StEvent input and writing out StEvent output for Emc
//
// Revision 1.1  2000/05/15 21:18:32  subhasis
// initial version
//
// tower-smd-psd-track matching Maker for EMC
//
//
// Authors: Subhasis Chattopadhyay.
//
#ifndef STAR_StEpcMaker
#define STAR_StEpcMaker
#include "StMaker.h"
#include "StMessMgr.h"
#include <TH2.h>

class StEvent;
class StEmcCollection;
class StPointCollection;

class StEpcMaker : public StMaker
{
private:
    StEvent*             mEvent;
    StEmcCollection*     mTheEmcCollection;
    StPointCollection*   mPoint;
    void                 MakeHistograms();    // Filling QA Histograms

protected:
    TH1F *m_point_flag;        //! //Point Flag spectra
    TH1F *m_point_quality;        //! //Point quality spectra
    TH1F *m_point_energy[4];   //! //Point Energy spectra
    TH1F *m_point_eta[4];      //! //Point Eta spectra
    TH1F *m_point_phi[4];      //! //Point Phi spectra
    // the following histograms will be created and filled only if the
    // mFilHisto is set to kTRUE
    TH1F *m_point_sigeta[4];   //! //Point SigmaEta spectra
    TH1F *m_point_sigphi[4];   //! //Point SigmaPhi spectra
    TH1F *m_point_trmom[4];    //! //Point TrMom spectra
    TH1F *m_emc_points[4];     //! //Emc Point multiplicity

    Bool_t mFillHisto;

public:
    StEpcMaker(const char *name="epc");
    virtual ~StEpcMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    virtual Int_t fillStEvent();
    void    setPrint(Bool_t a)
    {
		LOG_INFO << "::setPrint() is obsolete.  Use logger config file to set verbosity instead." << endm;
    }///< Obsolete function; users can control messages with logger config file.
    void  setFillHisto(Bool_t a)
    {
        mFillHisto = a;
    } ///< Turns on/off histogram filling
    virtual void  Browse(TBrowser* b); // StEvent staf will be visible in browser

    virtual const char *GetCVS() const
    {
        static const char cvs[]=
            "Tag $Name:  $ $Id: StEpcMaker.h,v 1.13 2014/08/06 11:43:08 jeromel Exp $ built " __DATE__ " " __TIME__ ;
        return cvs;
    }

    ClassDef(StEpcMaker,0)// EMC-Track match maker
};

#endif








