// @(#)root/base:$Name:  $:$Id: TVirtualViewer3D.cxx
// Author: Olivier Couet 05/10/2004

/*************************************************************************
 * Copyright (C) 1995-2004, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TObject3DViewFactoryABC                                              //
//                                                                      //
// Abstract 3D shapes viewer. The concrete implementations are:         //
//                                                                      //
// TViewerX3D   : X3d viewer                                            //
// TViewerOpenGL: OpenGL viewer                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TObject3DViewFactoryABC.h"

#include <assert.h>

#include "TROOT.h"
#include "TPluginManager.h"
#include "TError.h"


#include <utility>	// makepair
#include <list>
#include <map>

//std::map<TString, TObject3DViewFactoryABC *> TObject3DViewFactoryABC::gfMap;

//typedef std::pair<TString, TObject3DViewFactoryABC *> TFactoryStringPair;
//std::list<TFactoryStringPair> gFList;
std::map<TString, TObject3DViewFactoryABC *> gFMap;

ClassImp(TObject3DViewFactoryABC)
//______________________________________________________________________________
void TObject3DViewFactoryABC::DestroyCurrentView3DFactory(Option_t * /* type */ ) 
{
   assert(0);
}

//______________________________________________________________________________
void TObject3DViewFactoryABC::Registr(TObject3DViewFactoryABC * f, const char * name)
{
	gFMap[name] = f;
}

//______________________________________________________________________________
void TObject3DViewFactoryABC::Unregistr(const char * name)
{
   if (name) {
    /*
     TString factoryName(type);
      gfMap.insert(FACTORY_PAIR(factoryName,factory));	
   */
   }
}

//______________________________________________________________________________
TObject3DViewFactoryABC* TObject3DViewFactoryABC::View3DFactory(Option_t *type)
{
 //  Create a Viewer 3D of specified type
   TObject3DViewFactoryABC *factory = 0;
   std::map<TString, TObject3DViewFactoryABC *>::iterator f = gFMap.find(TString(type));
   if (f != gFMap.end() )
         factory = gFMap.find(TString(type))->second;
	   /*
#if 0
      TPluginHandler *h;
      if ((h = gROOT->GetPluginManager()->FindHandler("TObject3DViewFactoryABC", type))) {
         if (h->LoadPlugin() == -1) return 0;    
         factory  = (TObject3DViewFactoryABC *) h->ExecPlugin(0); 
         typedef std::pair <TString, TObject3DViewFactoryABC *> FACTORY_PAIR;
         TString factoryName(type); 
         gfMap.insert(FACTORY_PAIR(factoryName,factory));
      }
#else
	   */
      //printf("selecting view factory: %s\n", type);
      //   g3DFactory  = new TObjectOpenGLViewFactory();
      //g3DCoinFactory  = new TObjectCoinViewFactory();
      //g3DOGLFactory  = new TObjectOpenGLViewFactory();
      
//         typedef std::pair <TString, TObject3DViewFactoryABC *> FACTORY_PAIR;
//         TString factoryName(type); 
//         gfMap.insert(FACTORY_PAIR(factoryName,factory));
//#endif
   return factory;
}

// ===== FactoryI =============================================================

//typedef std::pair<const char *, TObject3DViewFactoryABC *> TFactoryStringPair;

//std::list<TFactoryStringPair> * gFList = 0;
//View3DPlan<TObjectOpenGLViewFactory> gF("ogl");		// registration

/*
ClassImp(FactoryI)
//______________________________________________________________________________
void FactoryI::RegisterMe(const char * str)
{
	
   if (gList == 0) {
      gList = new std::list<TFactoryStringPair>;
   }
   glist->push_back(std::make_pair(str, f));
	
}
*/
/*
//______________________________________________________________________________
template<class T>
TObject3DViewFactoryABC * View3DPlan<T>::Create(const char * str)
{
	
   if (gList->empty()) {
      delete gList;
   }
}*/
