//*-- Author : Jan Balewski
// 
// JB 4/3/01 search for only firts leading charge particle (was also second)
// 
// $Id: getGeneratedLP.cxx,v 1.1 2001/04/12 15:19:09 balewski Exp $
// $Log: getGeneratedLP.cxx,v $
// Revision 1.1  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//   Access pointer to the PYTHIA track of the charged leading particle
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <assert.h>
#include <math.h>
#include <strings.h>

#include "StppLPevalMaker.h"
#include "tables/St_g2t_track_Table.h" 


//_____________________________________________________________
//_____________________________________________________________
//_____________________________________________________________
void StppLPevalMaker::getGeneratedLP(St_g2t_track *gtra, g2t_track_st *&GLP)
{// select 1st leading charge particle from the PYTHIA primary tracks
 // within the TPC acceptance 

 assert(gtra);
 
 // select only tracks of interests
 g2t_track_st *GTRA=gtra->GetTable(); assert(GTRA);
 int it;
 float pT1=0;

 // search for the largest two pT values 
 for( it=0; it<gtra->GetNRows();it++,GTRA++) 
   { 
     if(GTRA->eg_label<=0 ) break; //abort on first secondary/pileup  particle

     if( fabs(GTRA->charge) <0.5 ) continue; // kill charg=0.
     if( fabs(GTRA->eta) >EtaCut ) continue; // kill too large eta
     //printf("it=%d pid=%d nTPH=%d eta=%f label=%d pt=%f m=%f, m2=%f\n",it,GTRA->eg_pid,GTRA->n_tpc_hit,GTRA->eta,GTRA->eg_label,GTRA->pt,pT1,pT2);

     if(GTRA->pt<pT1) continue;
     pT1=GTRA->pt;
     GLP=GTRA;
     
     //printf("pTmax=%f,  id=%d, eta=%f\n",pT1, GLP[0]->id, GLP[0]->eta);
   } // all track were examined
  
 printf("getGeneratedLP(), Nprim=%d of Ntable=%d, found largest pT=%f GeV/c \n",it,(int)gtra->GetNRows(),pT1);
 return ; 
}






