//  modified by JB 2/2/01: trigOnCtb() isolated and upgraded
// 
//*-- Author : George , Jan Balewski 
// $Id: StppPionMaker.cxx,v 1.1 2001/05/03 23:38:10 balewski Exp $
// $Log: StppPionMaker.cxx,v $
// Revision 1.1  2001/05/03 23:38:10  balewski
// *** empty log message ***
//
// Revision 1.7  2001/04/23 19:44:26  balewski
// *** empty log message ***
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
//  Emulates trigger response for the M-C data
// 
//                                                                     
//////////////////////////////////////////////////////////////////////////
#include <assert.h>

#include <math.h>
#include <strings.h>
#include <stdio.h>
#include <stdlib.h> 

#include "StppPionMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "TH1.h"
#include "TH2.h"

#include "StppEmcDst.h"  

ClassImp(StppPionMaker)

//_____________________________________________________________________________
//_____________________________________________________________________________
StppPionMaker::StppPionMaker(const char *name):StMaker(name){
 //  const char *name -  the name of this constructor
  printf("CCCCCCCCCCCCCCC Constructor of class=%s= executed\n", name);
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
StppPionMaker::~StppPionMaker(){  
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StppPionMaker::Init(){
  //  Init - empty
  printf("InInInInInInInInInIn    Initialization start \"%s\",  m_Mode=%d... \n", GetName(),m_Mode);
  return StMaker::Init();
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StppPionMaker::Make(){
 
  printf(" Mmmmmmmmmmmmmmmmmmmmmm   start maker ::%s\n",GetName());

  ppEmcDst_t row;
  printf("new ppEmcDst row\n");

  row.Eta=777;
 
  // add it to dst-tree
  ppEmcDst *my=new ppEmcDst("pi0",1); // name of the table in ppDst
  my->AddAt(&row);

  St_DataSet *dst1 = GetDataSet("dst");
  assert(dst1);
  dst1->Add(my);

  return kStOK;
}


