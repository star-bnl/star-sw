// $Id: St_dst_Maker.cxx,v 1.1 1999/01/02 19:09:22 fisyak Exp $
// $Log: St_dst_Maker.cxx,v $
// Revision 1.1  1999/01/02 19:09:22  fisyak
// Add Clones
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:29  perev
// cleanup
//
// Revision 1.5  1998/10/02 13:46:08  fine
// DataSet->DataSetIter
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_dst_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "St_dst_Maker.h"
#include "StChain.h"
#include "St_ObjectSet.h"
#include "St_DataSetIter.h"
#include "TRandom.h"
#include "TSystem.h"
#include "StVertex.h"
#include "StTrack.h"
//#include "StPoint.h"

const Int_t St_dst_Maker::maxNoVertex = 100000;
const Int_t St_dst_Maker::maxNoTrack  = 100000;
const Int_t St_dst_Maker::maxNoPoint  = 200000;
//TClonesArray St_dst_Maker::*m_Vertex = 0;
//TClonesArray St_dst_Maker::*m_Track = 0;

ClassImp(St_dst_Maker)

//_____________________________________________________________________________
St_dst_Maker::St_dst_Maker(const char *name, const char *title):StMaker(name,title){
   drawinit=kFALSE;
}
//_____________________________________________________________________________
St_dst_Maker::~St_dst_Maker(){
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Init(){
// Create CloneArrays
  if (! m_Vertex) m_Vertex = new TClonesArray("StVertex",maxNoVertex); // vertex
  if (! m_Track)  m_Track  = new TClonesArray("StTrack", maxNoTrack);  // globtrk globtrk_aux dst_dedx
  //  if (! m_Point)  m_Point  = new TClonesArray("StDstPoint", maxNoPoint);  // point

// Create Histograms    
   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_dst_Maker::Make(){
//  PrintInfo();
         //  Create and Fill the Track objects
  TClonesArray &vertices = *m_Vertex;
  TClonesArray &tracks   = *m_Track;
  Char_t VertexName[80];
  Char_t TrackName[80];
  Int_t  Nchar;
  St_DataSetIter dst(gStChain->DataSet("global"));
  if (dst.Cd("dst")) {
    StVertex *vertex = 0;
    dst_vertex_st *vert = 0;
    St_dst_vertex *dst_vertex = (St_dst_vertex *) dst("vertex");
    if (dst_vertex) {
      vert = dst_vertex->GetTable();
      for (Int_t fN=0; fN< dst_vertex->GetNRows(); fN++, fNvertex++, vert++){
	Nchar = sprintf(VertexName,"Vertex_%i",vert->id);
	//	vertex = new (vertices[m_Vertex->IndexOf(0)]) StVertex(VertexName);
	vertex = new (vertices[vert->id]) StVertex(VertexName);
	*vertex = *vert;
        m_DataSet->Add(new St_ObjectSet(vertex));
      }
    }
    St_dst_track *prim_track = (St_dst_track *) dst("primtrk");
    if (prim_track) {
      dst_track_st *trck = prim_track->GetTable();
      for (Int_t i=0; i< prim_track->GetNRows(); i++, fNtrack++, trck++){
	Nchar = sprintf(TrackName,"Track_%i_%i",trck->id_start_vertex,fNtrack);
	//        StTrack *track = new(tracks[m_Track->IndexOf(0)]) StTrack(TrackName);
        StTrack *track = new(tracks[fNtrack]) StTrack(TrackName);
	*track = *trck;
        Nchar = sprintf(VertexName,"Vertex_%i",trck->id_start_vertex);
	St_ObjectSet *vert = (St_ObjectSet *) dst(VertexName);
        if (vert){
	  vert->Add( new St_ObjectSet(track));
	  ((StVertex *)vert->GetObject())->AddNtrack();
	}
	else {
          m_DataSet->Add( new St_ObjectSet(track));
	}
      }
    }
  }
  if (!m_DataSet->GetList())  {//if DataSet is empty fill it
  }
 return kStOK;
}
//_____________________________________________________________________________
void St_dst_Maker::MakeBranch()
{
//   Adds the list of physics objects to the ATLFast tree as a new branch

   if (m_Save == 0) return;

   TTree *tree = Tree();
   if (!tree) return;

   Int_t buffersize = 4000;

   if (m_Vertex && m_Vertex->InheritsFrom("TClonesArray")) 
       tree->Branch("vertex", &m_Vertex, buffersize);
    
   if (m_Track && m_Track->InheritsFrom("TClonesArray")) 
       tree->Branch("track", &m_Track, buffersize);

   StMaker::MakeBranch();
}
//_____________________________________________________________________________
void St_dst_Maker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: St_dst_Maker.cxx,v 1.1 1999/01/02 19:09:22 fisyak Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
void St_dst_Maker::Clear(Option_t *option)
{
 SafeDelete(m_DataSet);
 m_Vertex->Delete();
 m_Track->Delete();
}
//_____________________________________________________________________________
void St_dst_Maker::SetBranch()
{
    TTree *tree = Tree();
    if (!tree) return;

    TBranch *cloneBranch = tree->GetBranch("vertex");
    if (cloneBranch) cloneBranch->SetAddress(& m_Vertex);

    cloneBranch = tree->GetBranch("track");
    if (cloneBranch) cloneBranch->SetAddress(&m_Track);

    StMaker::SetBranch();
}
