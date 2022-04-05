#include "StFmsCalibMakerQa.h"
ClassImp(StFmsCalibMakerQa);

#include <TBranch.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TLeaf.h>
#include <TTree.h>

using namespace std;

//--------------------------------------------------------
void StFmsCalibMakerQa::CreateQaHist(int detId, int maxCh)
{
	if (detId == 8) mH1_nEvents = new TH1F("nEvents", ";runNo", 16094000-16066000, 16066000-0.5, 16094000-0.5);

	mH2_massWide[detId-8] = new TH2F(Form("mass_d%i_wide", detId), ";ch;mass", maxCh,0.5,maxCh+0.5, 100,0.,1.0);
    mH2_massWide[detId-8]->Sumw2();

	for (int i=0; i<7; i++)
    {
		mH2_massOpenA[detId-8][i] = new TH2F(Form("mass_d%i_openA%i",detId,i), "", 500,0.,0.05, 100,0.,1.);
		mH2_massOpenA[detId-8][i]->SetTitle(Form("%2.1f < Zgg < %2.1f;openA;mass", 0.1*i, 0.1*(i+1)));
		mH2_massOpenA[detId-8][i]->Sumw2();
	}

	for (int i=0; i<7; i++)
    {
		mH2_massPairE[detId-8][i] = new TH2F(Form("mass_d%i_pairE%i",detId,i), "", 1000,0,100, 100,0.,1.);
		mH2_massPairE[detId-8][i]->SetTitle(Form("%2.1f < Zgg < %2.1f;pairE;mass", 0.1*i, 0.1*(i+1)));
		mH2_massPairE[detId-8][i]->Sumw2();
	}

	for (int i=0; i<7; i++)
	{
		const char* Title = (i!=6)?Form("%2i < pair E < %2i", 20+10*i, 20+10*(i+1)):Form("%2i < pair E", 20+10*i);
		mH2_massZgg[detId-8][i] = new TH2F(Form("mass_d%i_zgg%i",detId,i), "", 100,0,1, 100,0.,1.);
		mH2_massZgg[detId-8][i]->SetTitle(Title);
		mH2_massZgg[detId-8][i]->Sumw2();
	}

	mH2_pointsEP[detId-8] = new TH2F(Form("pointsEP_d%i", detId), ";eta;phi", 72,2.5,4.3, 120,-M_PI,M_PI);
	mH2_pointsEP[detId-8]->Sumw2();

	if (detId == 11)
	{
		for (int i=0; i<2; i++) //Point 0 or 1
		for (int j=0; j<3; j++) //All, Large, and Small
	    {
			const int   tBin = (j<2)?210:110;
	   	    const float tEnd = (float)tBin/2;
	        mH2_pointsXY[i][j] = new TH2F(Form("point%iXY_%i", i, j), ";X;Y", tBin,-tEnd,tEnd, tBin,-tEnd,tEnd);
			mH2_pointsXY[i][j]->SetTitle(Form("Point %i", i));
	        mH2_pointsXY[i][j]->Sumw2();
		}
	}

	return;
}//CreateQaHist

//-----------------------------------------------------------------------
void StFmsCalibMakerQa::CreateQaHistAdc(int detId, int maxCh, int trigId)
{
	if (detId == 8) mH1_trig = new TH1F("trig", ";TrigId", 1000, trigId-500, trigId+500);

	mH2_adc[detId-8] = new TH2F(Form("adc_d%i", detId), "", maxCh,0.5,maxCh+0.5, 250,0,250);
	mH2_adc[detId-8]->SetTitle(Form("detId=%i, Triggered by %i;ch;adc", detId, trigId));
	mH2_adc[detId-8]->Sumw2();

	mH2_adcWide[detId-8] = new TH2F(Form("adc_d%i_wide", detId), "", maxCh,0.5,maxCh+0.5, 5000,0,5000);
	mH2_adcWide[detId-8]->SetTitle(Form("detId=%i, Triggered by %i, extended range;ch;adc", detId, trigId));
	mH2_adcWide[detId-8]->Sumw2();

	return;
}//CreateQaHistAdc

//--------------------------------------------
void StFmsCalibMakerQa::CreateQaHistZVtx(void)
{
    mH1_bbcZ = new TH1F("bbcZ", "Bbc slewing corrected;bbcZ", 1000, -250, 250);
    mH1_bbcZ->Sumw2();
    mH1_diffMass = new TH1F("diff_mass", ";#DeltaMass (default - corr))", 800, -0.2, 0.2);
    mH1_diffMass->Sumw2();
    mH1_diffOpenA = new TH1F("diff_openA", ";#DeltaOpenA (default - corr)", 800, -0.04, 0.04);
    mH1_diffOpenA->Sumw2();

	return;
}//CreateQaHistZVtx

//----------------------------------------
void StFmsCalibMakerQa::CreateQaTree(void)
{
	cout <<Form("\nCreating a tree for QA: large disk space will be used...") <<endl;

	int iLeaf = 0;
	T = new TTree("T", "QA tree for StFmsCalibMaker");

	T->Branch("hits", 0, "nHit/s:detId[nHit]/s:ch[nHit]/s:pointB[nHit]/s:hitE[nHit]/F");
	((TLeaf*)T->GetBranch("hits")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mNhit);   iLeaf++;
    ((TLeaf*)T->GetBranch("hits")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mDetId);  iLeaf++;
    ((TLeaf*)T->GetBranch("hits")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mCh);     iLeaf++;
    ((TLeaf*)T->GetBranch("hits")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mPointB); iLeaf++;
    ((TLeaf*)T->GetBranch("hits")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mHitE);   iLeaf=0;

	T->Branch("clusters", 0, "cluTowers[2]/s:cluMax[2]/F:cluMin[2]/F:cluX[2]/F:cluY[2]/F");
	((TLeaf*)T->GetBranch("clusters")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mCluTowers); iLeaf++;
    ((TLeaf*)T->GetBranch("clusters")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mCluMax);    iLeaf++;
    ((TLeaf*)T->GetBranch("clusters")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mCluMin);    iLeaf++;
	((TLeaf*)T->GetBranch("clusters")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mCluX);      iLeaf++;
	((TLeaf*)T->GetBranch("clusters")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mCluY);      iLeaf=0;

	T->Branch("points", 0, "pointE[2]/F:pointX[2]/F:pointY[2]/F");
	((TLeaf*)T->GetBranch("points")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mPointE); iLeaf++;
	((TLeaf*)T->GetBranch("points")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mPointX); iLeaf++;
	((TLeaf*)T->GetBranch("points")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mPointY); iLeaf=0;

	T->Branch("pair", 0, "mass/F:openA/F:zgg/F");
    ((TLeaf*)T->GetBranch("pair")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mMass);  iLeaf++;
    ((TLeaf*)T->GetBranch("pair")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mOpenA); iLeaf++;
    ((TLeaf*)T->GetBranch("pair")->GetListOfLeaves()->At(iLeaf))->SetAddress(&mZgg);   iLeaf=0;

	T->Branch("trig", &mTrigBit, "trig/I");

	ResetQaTree();
	return;
}//CreateQaTree

//---------------------------------------
void StFmsCalibMakerQa::ResetQaTree(void)
{
	mNhit = 0;
	for (int i=0; i<mNhitMax; i++)
	{
		mDetId[i]  = -999;
		mCh[i]     = -999;
		mPointB[i] = -999;
		mHitE[i]   = -999.;
	}

	for (int i=0; i<2; i++)
	{
		mCluTowers[i] = -999;
		mCluMax[i]    = -999.; 
		mCluMin[i]    = -999.;
		mCluX[i]      = -999.;
		mCluY[i]      = -999.;

		mPointE[i] = -999.;
		mPointX[i] = -999.;
		mPointY[i] = -999.;
	}

	mMass  = -999.;
	mOpenA = -999.;
	mZgg   = -999.;

	mTrigBit = -999;

	return;
}//ResetQaTree
