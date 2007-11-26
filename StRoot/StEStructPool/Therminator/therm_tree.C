/******************************************************************************
 *                      T H E R M I N A T O R                                 *
 *                   THERMal heavy-IoN generATOR                              *
 *                           version 1.0                                      *
 *                                                                            *
 * Authors of the model: Wojciech Broniowski, Wojciech.Broniowski@ifj.edu.pl, *
 *                       Wojciech Florkowski, Wojciech.Florkowski@ifj.edu.pl  *
 * Authors of the code:  Adam Kisiel, kisiel@if.pw.edu.pl                     *
 *                       Tomasz Taluc, ttaluc@if.pw.edu.pl                    *
 * Code designers: Adam Kisiel, Tomasz Taluc, Wojciech Broniowski,            *
 *                 Wojciech Florkowski                                        *
 *                                                                            *
 * For the detailed description of the program and furhter references         * 
 * to the description of the model plesase refer to: nucl-th/0504047,         *
 * accessibile at: http://www.arxiv.org/nucl-th/0504047                       *
 *                                                                            *
 * Homepage: http://hirg.if.pw.edu.pl/en/therminator/                         *
 *                                                                            *
 * This code can be freely used and redistributed. However if you decide to   *
 * make modifications to the code, please contact the authors, especially     *
 * if you plan to publish the results obtained with such modified code.       *
 * Any publication of results obtained using this code must include the       *
 * reference to nucl-th/0504047 and the published version of it, when         *
 * available.                                                                 *
 *                                                                            *
 *****************************************************************************/
#include <TH1D.h>
#include <TH2D.h>
#include <TFile.h>
#include <TTree.h>
#include <fstream>
#include <iostream>
#include <sstream>

using namespace std;

typedef struct {Float_t px,py,pz,e,x,y,z,t,pid,mass,fatherpid,rootpid,event;} PARTICLE;
typedef struct 
{
  Int_t mPid;
  Int_t mFatherNum;
} DecayTreeNode;
  
DecayTreeNode sEvBuf[20000];

int findfather(int aFirstnum)
{
  if (sEvBuf[aFirstnum].mFatherNum > -1) 
    return sEvBuf[sEvBuf[aFirstnum].mFatherNum].mPid;
  else
    return sEvBuf[aFirstnum].mPid;
}

int findroot(int aFirstnum)
{
  int iter = aFirstnum;
  
  while (sEvBuf[iter].mFatherNum > -1) {
    iter = sEvBuf[iter].mFatherNum;
  }
  
  return sEvBuf[iter].mPid;
}

int main(int argc, char **argv)
{
  static PARTICLE tParticle;
  double kGeVToFm = 0.197327;
  TString tTreeName("tree");
  TString tInputFileName("event");

  if (argc>1)
    tInputFileName = argv[1];

  
  
  if (tInputFileName.EndsWith(".out")) {
    tTreeName = tInputFileName(0,tInputFileName.Length()-4);
  }
  else {
    tTreeName = tInputFileName;
    tInputFileName += ".out";
  }

  Double_t id, px, py, pz, p0, pt, mass, rx, ry, rz, rt, rap, En;
  Int_t fatherpid, part,ipart,event = 0, tNFile=1;
   
  ifstream tInFileStream(tInputFileName.Data());
  if (!tInFileStream.is_open()) {
    cout << "Error !" << endl << "The file " << tInputFileName.Data() << " does not exist or cannot be opened." << endl;
    exit(0);
  }

  istringstream *tInStringStreamBuffer;
  
  cout << "Reading " << tInputFileName.Data() << endl;
  char tCharBuf[500];
  
  TTree *tOutputTree;
  TFile *tOutputFile;

  if (tInFileStream) {
    char tNFile = '0';

    tInFileStream.ignore(500,'\n');
    tInFileStream.ignore(500,'\n');
    tInFileStream.ignore(500,'\n');

    while (tInFileStream.getline(tCharBuf,500))	{
      if (tInFileStream.gcount() < 50.0) {
	if (!(event%500)) {
	  if (event != 0) {
	    cout << "\rFinishing file " << tNFile << endl;
	    
	    tOutputFile->cd();
	    tOutputTree->Write();
	    tOutputFile->Close();
	  }
		  
	  tNFile++;
	  TString tFileFullName;
	  tFileFullName = tTreeName;
	  tFileFullName += tNFile;
	  tFileFullName += ".root";
	  
	  cout << "Creating file " << tFileFullName.Data() << endl;
		  
	  tOutputFile = new TFile(tFileFullName.Data(),"RECREATE");
	  tOutputFile->cd();
	  tOutputTree = new TTree("particles","Particle tree");
	  tOutputTree->Branch("part",&tParticle,"px:py:pz:e:x:y:z:t:pid:mass:fatherpid:rootpid:event");
	}
		
	cout << "\rEvent " << event++ << "      ";
	cout.flush();
	continue;
      }

      tInStringStreamBuffer = new istringstream(tCharBuf);
      (*tInStringStreamBuffer)>>ipart>>id>>px>>py>>pz>>En>>mass>>rx>>ry>>rz>>rt>>fatherpid>>part;
	    
      tParticle.px = px;
      tParticle.py = py;
      tParticle.pz = pz;
      tParticle.e  = En;
		  
      tParticle.x  = rx * kGeVToFm;
      tParticle.y  = ry * kGeVToFm;
      tParticle.z  = rz * kGeVToFm;
      tParticle.t  = rt * kGeVToFm;
		  
      tParticle.pid = id;

      tParticle.mass = mass;
		  
      sEvBuf[ipart].mPid = id;
      if (fatherpid > -1)
	sEvBuf[ipart].mFatherNum = fatherpid;
      else
	sEvBuf[ipart].mFatherNum = -1;
		  
      tParticle.fatherpid = findfather(ipart);
      tParticle.rootpid   = findroot(ipart);
      tParticle.event = event;

      if (event > 0 )
	tOutputTree->Fill();

      delete tInStringStreamBuffer;
    }
  }

  tOutputFile->cd();
  tOutputTree->Write();
  cout << endl;
}
