// $Id: ExampleReader.C,v 1.2 2008/12/29 16:12:48 kocolosk Exp $

/*****************************************************************************
 * @author Adam Kocoloski
 *
 * Quick example showing how to read StChargedPionEvent trees.
 *****************************************************************************/

void ExampleReader(const char *path = "/star/institutions/mit/common/run6-trans/chargedPions/chargedPions_7097009.tree.root") {
    gROOT->Macro("StRoot/StSpinPool/StChargedPionAnalysisMaker/macros/LoadLibs.C");
    
    TFile *f = TFile::Open(path);
    TTree *tree = (TTree*)f->Get("tree");
    StChargedPionEvent *event = new StChargedPionEvent;
    tree->SetBranchAddress("event", &event);
    
    for(int i=0; i<3; i++) {
        tree->GetEntry(i);
        
        cout << "run: " << event->runId() << "  event: " << event->eventId();
        
        if( event->vertex(0) )
            cout << "  vz: " << event->vertex(0)->Z() << endl;
        
        for(int j=0; j<event->nJets(); j++) {
            cout << "  jet #" << j+1 << "  ";
            cout << "pt: " << event->jet(j)->Pt() << "  "
                 << "eta: " << event->jet(j)->Eta() << "  "
                 << "phi: " << event->jet(j)->Phi() << endl;
            
            int nparticles = (event->jet(j)->particles()).size();
            for(int k=0; k<nparticles; k++) {
                StChargedPionJetParticle& p(event->jet(j)->particles()[k]);
                cout << "    particle #" << k+1 << "  "
                     << "  charge " << p.charge() << "  "
                     << "  pt " << p.Pt() << "  "
                     << "  eta " << p.Eta() << endl;
            }
        }
    }
}

/*****************************************************************************
 * $Log: ExampleReader.C,v $
 * Revision 1.2  2008/12/29 16:12:48  kocolosk
 * added $Id$/$Log$ as needed
 *
 *****************************************************************************/

