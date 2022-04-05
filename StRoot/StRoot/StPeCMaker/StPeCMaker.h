// $Id: StPeCMaker.h,v 1.22 2015/07/23 11:52:27 ramdebbe Exp $
//
// $Log: StPeCMaker.h,v $
// Revision 1.22  2015/07/23 11:52:27  ramdebbe
// added flag to suppress tree to output and includes summary histograms
//
// Revision 1.21  2015/02/25 01:19:45  ramdebbe
// added a setter to select writing Roman Pot Collection to output tree
//
// Revision 1.20  2014/08/06 11:43:32  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.19  2013/12/27 16:45:16  ramdebbe
// added extrapolation of tracks to TOF, StBTofGeometry and St_geant_Maker with //!
//
// Revision 1.18  2013/01/24 15:43:14  ramdebbe
// added more flags to choose input or output tracks tof etc.
//
// Revision 1.17  2012/07/03 19:38:09  ramdebbe
// raised ClassDef from 1 to 2
//
// Revision 1.16  2012/06/13 15:45:46  ramdebbe
// Added flags to include TOF and Vertex branches in tree
//
// Revision 1.15  2003/11/25 01:54:32  meissner
// correct several bugs: eta cut for tracks, charge sorting, add counting of FTPC and TPC primary tracks, Add bbc information
//
// Revision 1.14  2003/09/10 19:47:25  perev
// ansi corrs
//
// Revision 1.13  2002/12/19 18:09:53  yepes
// MuDST input added
//
// Revision 1.12  2002/04/18 19:02:12  meissner
// Change Init to  InitRun
//
// Revision 1.11  2001/09/14 18:00:22  perev
// Removed references to StRun.
//
// Revision 1.10  2001/02/12 21:15:57  yepes
// New version of StPeCMaker, lots of changes
//
// Revision 1.7  2000/04/21 19:09:42  nystrand
// Update StPeCPair class, new histograms
//
// Revision 1.6  2000/03/24 22:36:16  nystrand
// First version with StPeCEvent
//
// Revision 1.5  2000/01/20 23:03:15  nystrand
// First Version of StPeCMaker with new StEvent
//
// Revision 1.3  1999/07/15 13:57:21  perev
// cleanup
//
// Revision 1.2  1999/04/08 16:37:27  nystrand
// MakeBranch,SetBranch removed
//
// Revision 1.1  1999/04/06 20:47:35  akio
// The first version
//
// Revision 1.0  1999/03/05 11:00:00  Nystrand
// First Version
//
///////////////////////////////////////////////////////////////////////////////
//
// StPeCMaker
//
// Description: 
//  Sample maker to access and analyze Peripheral Collisions through StEvent
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Joakim Nystrand, LBNL
//
// History:
//
///////////////////////////////////////////////////////////////////////////////
#ifndef StPeCMaker_HH
#define StPeCMaker_HH
#include "StMaker.h"
#include "StPeCEvent.h"
#include "StPeCTrigger.h"
#include "StPeCGeant.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
#include "StBTofUtil/StBTofGeometry.h"
#include "St_geant_Maker/St_geant_Maker.h"


class StEvent;
class StPeCEvent;
class StPeCPair;
class TH1F;
class TH2F;
class StMuDst;



class StPeCMaker : public StMaker
{
public:
	StPeCMaker(const Char_t *name = "analysis");
	virtual ~StPeCMaker();
	virtual Int_t Init();
	virtual Int_t InitRun(Int_t runnr);
	virtual Int_t Make();
	virtual Int_t Finish();

	void setInfoLevel(Int_t in) {infoLevel = in ;};
	void setFilter(Int_t fi) {filter = fi;};
	void setMuDst(StMuDst* mu) {muDst = mu;};	//Accessor for muDst pointer
	void setFileName ( TString name ) { treeFileName = name ; } ;
	void setUseBemc   ( Bool_t includeBemc   = kFALSE ) { useBemc   = includeBemc ; } ;
	void setUseTOF    ( Bool_t includeTOF    = kFALSE ) { useTOF    = includeTOF ; } ;
	void setUseVertex ( Bool_t includeVertex = kFALSE ) { useVertex = includeVertex ; } ;
	void setUseTracks ( Bool_t includeTracks = kFALSE ) { useTracks = includeTracks ; } ;
	void setRomanPots ( Bool_t includeRP     = kFALSE ) { useRP     = includeRP ; } ;

	void setReadStMuDst     ( Bool_t includeStMuDst = kFALSE ) { readStMuDst             = includeStMuDst ; } ;
	void setReadStEvent     ( Bool_t includeStEvent = kFALSE ) { readStEvent             = includeStEvent ; } ;
	void setReadBothInputs  ( Bool_t includeBoth = kFALSE )    { readStMuDst_and_StEvent = includeBoth ; } ;
	void setSuppressTreeOut ( Bool_t suppressTree = kFALSE )   { treeOff = suppressTree ; } ;

	void setOutputPerRun ( Int_t in = 1 ) { outputPerRun = in ; } ;

	void setStarHall ( TVolume * pointer ) { mstarHall = pointer ; } ;

	TString treeFileName ;
	Bool_t   useBemc;     //if TRUE BEMC information is written to ntuple
	Bool_t   useTOF;      //if TRUE TOF information is written to ntuple
	Bool_t   useVertex;   //if TRUE Vertex information is written to ntuple
	Bool_t   useTracks;   //if TRUE track information is written to ntuple
	Bool_t   useRP;       //if TRUE Roman Pot information is written to ntuple

	Bool_t readStMuDst;               //if TRUE will work with information passed in StMuDst format
	Bool_t readStEvent;               //if TRUE will work with information passed in StEvent format
	Bool_t readStMuDst_and_StEvent;   //if TRUE will work with information passed in both StMuDst and StEvent format
	Bool_t treeOff;                   //if TRUE the pico dst TTree will not be present in output files

	void setTriggerOfInterest ( const char * selectTrigger = "UPC_Main" ) { triggerChoice = selectTrigger ; } ;

	int countAcceptedVtx ;
	TH1F * hNumVtx, * hNumAccVtx, * hNumAccWithTOF, * hNumRejecWithTOF, *  hNumTrkRejecVtx, * hzVertexAccTOF, * hzVertexRejectTOF;
	friend class StPeCPair;


protected:
	TFile* m_outfile;

	TTree* uDstTree;
	TTree* geantTree;

	StPeCEvent* pevent;
	StPeCTrigger* trigger;
	StBTofGeometry *                mBTofGeom; //! 
	St_geant_Maker *                geantU; //!    
	TVolume        *                mstarHall;
	StPeCGeant* geant;
 


	Int_t   infoLevel;
	Int_t   filter;	//1 == two prong, 2 == four prong
	Int_t   outputPerRun ; // 1=output per run 0(default)=one output file
	TList * fSnapShots;



private:
	StMuDst* muDst;
	string triggerChoice;             // default is UPC_Main,  it will recognize UPC_Topo, ZDC_Main


	Int_t Cuts(StEvent* event, StPeCEvent* pevent);
	Int_t Cuts4Prong(StEvent* event, StPeCEvent* pevent);
	Int_t triggerSim(StEvent*);

	virtual const char *GetCVS() const
	{static const char cvs[]="Tag $Name:  $ $Id: StPeCMaker.h,v 1.22 2015/07/23 11:52:27 ramdebbe Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

	ClassDef(StPeCMaker,2)
};

#endif



