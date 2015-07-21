#include "StHbtFlowPicoReader.h"
#include "StThreeVectorD.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "math_constants.h"
#include "SystemOfUnits.h"
#include <fstream>

ClassImp(StHbtFlowPicoReader)

  const double ETAMIN = -5.; /* cutoff values to avoid nan arithmetics */
  const double ETAMAX = 5.;
///////////////////////////////////////////////////////////////////
StHbtFlowPicoReader::StHbtFlowPicoReader(string list_inp,int nevents)
{
  /*
Because pairs analysis is so much slower than DWT, I have to make finer
parallelization splitting (more jobs). For this reason, I have two different
set of "to do list" files: one for the DWT analysis and the other for pairs.
It makes therefore no sense to assume that to_do_list.inp files (for DWT)
will be recycled by the pairs analysis, and consequently to maintain the 
uniformity of the input format. Here I follow the StHbtMuDstReader format
(.lis).
   */
  cout << "StHbtFlowPicoReader::StHbtFlowPicoReader Initializing the reader ! \n";
  to_do_list_inp = list_inp;
  mReaderStatus = 0;           // means "good"
  Nbytes = 0;
  crrnt_entry = 0;
  crrnt_eventI = 0;
  NEvt_in_chain = 0;
  NEVENTS = nevents;
  // read the list of input data files (DST ntuples)


   std::ifstream input(to_do_list_inp.c_str());

   while (input.good())
     { 
       string filename;
       input >>  filename;

       if (filename!="" )
         {
           L_runs.push_back(filename);
         }
     }

   input.close();
cout << " chain has "<<L_runs.size()<<" runs \n";

  Init();
}
///////////////////////////////////////////////////////////////////
int StHbtFlowPicoReader::Init()
{
  cout << " StHbtFlowPicoReader::Init\n"; // open DST

   // build the chain
DST_Chain = new TChain("FlowTree");

   vector<string>::const_iterator it = L_runs.begin();

 
   while (it!=L_runs.end())
     {
       if (NEvt_in_chain <NEVENTS)
         {
           cout << "StHbtFlowPicoReader::StHbtFlowPicoReader add " <<*it<<"\n";

           TChain* tmpChain = new TChain("FlowTree");

           int ok1 = tmpChain->Add((*it).c_str());
           if (!ok1) {cout << "tmpChain->Add error !\n";}
           int tmpEntries = (int)tmpChain->GetEntries();
           NEvt_in_chain += tmpEntries;
           cout << " in file " <<tmpEntries<<" total "<<NEvt_in_chain<< "\n";
           delete tmpChain;
           cout << " adding... \n";
           int ok2 = DST_Chain->Add((*it).c_str());
           if (!ok2) {cout << "DST_Chain->Add error !\n";}
         }
           ++it;
     }

      this->pDST = new flow_pDST(DST_Chain);
       cout << "ST_txtr::ST_txtr flow_pDST instantiated \n";
 this->pDST->fChain->SetBranchStatus("*",0);  // disable all branches

 // enable the branches we need 
 this->pDST->fChain->SetBranchStatus("mRunID",1);
 this->pDST->fChain->SetBranchStatus("mMagneticField",1);
 this->pDST->fChain->SetBranchStatus("mEventID",1);
 this->pDST->fChain->SetBranchStatus("mNtrack",1);
 this->pDST->fChain->SetBranchStatus("mVertexX",1);
 this->pDST->fChain->SetBranchStatus("mVertexY",1);
 this->pDST->fChain->SetBranchStatus("mVertexZ",1);
 this->pDST->fChain->SetBranchStatus("fTracks.fUniqueID",1);
 this->pDST->fChain->SetBranchStatus("fTracks.fBits",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mPtGlobal",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mEtaGlobal",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mPhiGlobal",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mNhits",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mCharge",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mFitPts",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mMaxPts",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mDedx",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mPidElectron",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mPidPion",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mPidKaon",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mPidProton",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mMostLikelihoodPID",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mMostLikelihoodProb",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mDcaGlobal",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mDcaGlobalX",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mDcaGlobalY",1);
 this->pDST->fChain->SetBranchStatus("fTracks.mDcaGlobalZ",1);

 Nentries = (int)(this->pDST->fChain->GetEntries());

 if (Nentries) {return 0;}
 else 
   {
     cout << "StHbtFlowPicoReader::Init() -- see no entries \n";
     return 1;
   }
}
///////////////////////////////////////////////////////////////////
StHbtEvent* StHbtFlowPicoReader::ReturnHbtEvent()
{

  /* using StHbtMcEventReader.cxx as an example
http://www.star.bnl.gov/webdatanfs/dox/html/StHbtMcEventReader_8cxx-source.html

StHbtTrack is here:
http://www.star.bnl.gov/webdatanfs/dox/html/StHbtTrack_8hh-source.html
http://www.star.bnl.gov/webdatanfs/dox/html/StHbtTrack_8cc-source.html

StHbtEvent:
http://www.star.bnl.gov/webdatanfs/dox/html/StHbtEvent_8hh-source.html
http://www.star.bnl.gov/webdatanfs/dox/html/StHbtEvent_8cc-source.html
   */
StHbtEvent* hbtEvent = new StHbtEvent();

 cout << "StHbtFlowPicoReader::ReturnHbtEvent() crrnt_entry begins "
<<crrnt_entry<<"\n";
 int ientry;
 if (crrnt_entry<Nentries)
   {
     ientry = this->pDST->LoadTree(crrnt_entry); 
     nb = this->pDST->fChain->GetEntry(crrnt_entry); Nbytes += nb; 
     
     hbtEvent->SetEventNumber(crrnt_eventI);
     hbtEvent->SetCtbMult(0);
     hbtEvent->SetZdcAdcEast(0);
     hbtEvent->SetZdcAdcWest(0);
     hbtEvent->SetNumberOfTpcHits(0);
     
     int Mult = this->pDST->fTracks_;
     hbtEvent->SetNumberOfTracks(Mult);
     hbtEvent->SetNumberOfGoodTracks(Mult);  
     hbtEvent->SetReactionPlane(0.);
     hbtEvent->SetReactionPlaneSubEventDifference(0.);
     
     float BTesla = pDST->mMagneticField;     
     float vX = pDST->mVertexX;
     float vY = pDST->mVertexY;
     float vZ = pDST->mVertexZ;
     
     StHbtThreeVector VertexPosition = StHbtThreeVector(vX,vY,vZ);

     hbtEvent->SetPrimVertPos(VertexPosition); 
     // loop over tracks

     int UncorrPositivePrim = 0; /* defined in StEventUtilities/StuRefMult.hh*/
     int UncorrNegativePrim = 0;

     for (short itrack = 0; itrack<Mult; itrack++)
       {
	 StHbtThreeVector tmpP;
	 float Phi = pDST->fTracks_mPhiGlobal[itrack];
	 float pT = pDST->fTracks_mPtGlobal[itrack];
	 float eta = pDST->fTracks_mEtaGlobal[itrack];
	 float pX = pT*cos(Phi);
	 float pY = pT*sin(Phi);
	 float expeta = exp(-eta);
	 float theta = 2*atan(expeta);

         short charge = pDST->fTracks_mCharge[itrack];
	 bool reject = false;
	 if (charge ==0) reject = true;
	 /* Sometimes eta is nan. Can we reject such tracks ? */
	 if ( !(eta>ETAMIN) || !(eta<ETAMAX)) reject = true;


	 if (!reject)
	   {
	 StHbtTrack* hbtTrack = new StHbtTrack();
	 hbtTrack->SetHiddenInfo(0);
	     //	     cout << " good eta "<<eta<<"\n";

	     /* zenith angle theta has to be non-negative, between 0 and Pi;
		atan changes between -Pi/2 and Pi/2.
	     */
	     
	     if (theta<0) {theta = C_PI+theta;}
	     float p = pT/sin(theta);
	     float pZ = p-pT*expeta; /* because expeta = tan(theta/2) 
					= (1-cos(theta))/sin(theta) = (p-pZ)/pT */
	     
	     tmpP.setX(pX);
	     tmpP.setY(pY);
	     tmpP.setZ(pZ);
	     hbtTrack->SetP(tmpP); 
	     
	     hbtTrack->SetTrackId(itrack); // ? Frank knows what it is
	     //         int PID=pDST->fTracks_mMostLikelihoodPID[itrack]);
	     float dEdx = pDST->fTracks_mDedx[itrack];

	     float ProbElectron = pDST->fTracks_mPidElectron[itrack];
	     float ProbPion = pDST->fTracks_mPidPion[itrack];
	     float ProbKaon = pDST->fTracks_mPidKaon[itrack];
	     float ProbProton = pDST->fTracks_mPidProton[itrack];
	     
	     hbtTrack->SetdEdx(dEdx);

	     hbtTrack->SetPidProbElectron(ProbElectron);

	     hbtTrack->SetPidProbPion(ProbPion);
	     hbtTrack->SetPidProbKaon(ProbKaon);
	     hbtTrack->SetPidProbProton(ProbProton);


	     int Nhits = pDST->fTracks_mNhits[itrack];
	     
	     float DCA = pDST->fTracks_mDcaGlobal[itrack];
	     float DCAX =  pDST->fTracks_mDcaGlobalX[itrack];
	     float DCAY =  pDST->fTracks_mDcaGlobalY[itrack];
	     float DCAZ =  pDST->fTracks_mDcaGlobalZ[itrack];
	     
	     float DCAXY = sqrt(DCAX*DCAX+DCAY*DCAY);
	     
	     hbtTrack->SetDCAxyGlobal(DCAXY);
	     hbtTrack->SetDCAzGlobal(DCAZ);
	     
	     if (Nhits>=10 && fabs(eta)<0.5 && DCA<3.)
	       {
		 if (charge>0) {UncorrPositivePrim++;}
		 if (charge<0) {UncorrNegativePrim++;}
	       }
	     hbtTrack->SetNHits(Nhits); 
	     hbtTrack->SetCharge(charge);
	     hbtTrack->SetNHitsPossible(pDST->fTracks_mMaxPts[itrack]);     
	     hbtTrack->SetPt(pT);
	     hbtTrack->SetPtGlobal(pT);

	     int gid =  pDST->fTracks_mMostLikelihoodPID[itrack];
	     float pid_prob = pDST->fTracks_mMostLikelihoodProb[itrack];

	     // Assume that this can only happen in MC
	     if (pid_prob == 1.)
	       {
		 if (gid ==2 || gid ==3) 
		   {
		     hbtTrack->SetNSigmaElectron(0.); 
		     hbtTrack->SetNSigmaPion(-1000.); 
		     hbtTrack->SetNSigmaKaon(-1000.); 
		     hbtTrack->SetNSigmaProton(-1000.); 
		   }
		 else if (gid==8 || gid ==9)
		   {
		     hbtTrack->SetNSigmaElectron(1000.); 
		     hbtTrack->SetNSigmaPion(0.); 
		     hbtTrack->SetNSigmaKaon(-1000.); 
		     hbtTrack->SetNSigmaProton(-1000.); 
		   }
		 else if (gid==11 || gid==12)
		   {
		     hbtTrack->SetNSigmaElectron(1000.); 
		     hbtTrack->SetNSigmaPion(1000.); 
		     hbtTrack->SetNSigmaKaon(0.); 
		     hbtTrack->SetNSigmaProton(-1000.); 
		   }
		 else if (gid==14 ||  gid==15)
		   {
		     hbtTrack->SetNSigmaElectron(1000.); 
		     hbtTrack->SetNSigmaPion(1000.); 
		     hbtTrack->SetNSigmaKaon(1000.); 
		     hbtTrack->SetNSigmaProton(0.); 
		   }
		 else
		   {
		     hbtTrack->SetNSigmaElectron(1000.); 
		     hbtTrack->SetNSigmaPion(0.); 
		     hbtTrack->SetNSigmaKaon(-1000.); 
		     hbtTrack->SetNSigmaProton(-1000.); 
		   }

	       }
	     
	     // define helix:
	     
	     const StPhysicalHelixD& mHelix = 
	       StPhysicalHelixD(tmpP,VertexPosition,BTesla*tesla,charge);
	     
	     hbtTrack->SetHelix(mHelix);
	     
	     hbtEvent->TrackCollection()->push_back(hbtTrack);
	   }
       }

     hbtEvent->SetUncorrectedNumberOfPositivePrimaries(UncorrPositivePrim);
     hbtEvent->SetUncorrectedNumberOfNegativePrimaries(UncorrNegativePrim);

     crrnt_entry++;
     crrnt_eventI++;
          cout << "StHbtFlowPicoReader return event # "<<crrnt_eventI<<"\n";
	  //	  hbtEvent->RandomizeTrackOrder();
     return hbtEvent;
   }
 else
   {
     mReaderStatus = 1;
     delete hbtEvent;
     return 0;
   }
}
///////////////////////////////////////////////////////////////////
/*
int StHbtFlowPicoReader::WriteHbtEvent(StHbtEvent*)
{
  cout << " calling my own  StHbtFlowPicoReader::WriteHbtEvent(StHbtEvent*)\n";
  return 0;
}
*/
///////////////////////////////////////////////////////////////////

StHbtString StHbtFlowPicoReader::Report()
{
return (StHbtString)" Here is the reader report \n";
}
///////////////////////////////////////////////////////////////////
StHbtFlowPicoReader::~StHbtFlowPicoReader()
{
}





