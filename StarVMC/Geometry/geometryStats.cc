#include <map>
#include <iostream>
#include <assert.h>

#include "TString.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoManager.h"

std::map< TString, Int_t > volume_hash;

Int_t numberOfNodes   = 0;
Int_t numberOfVolumes = 0;

Float_t weightA = 0.0;
Float_t weightS = 0.0;

Int_t getNumberOfNodes(){ return numberOfNodes; }
Int_t getNumberOfVolumes(){ return numberOfVolumes; }

Int_t countDaughters( TGeoNode *node )
{
  Int_t sum=0;
  TIter next(node->GetNodes());

  {
    TGeoNode *node = 0;
    while ((node=(TGeoNode*)next()))
      {
	TGeoVolume *volume=node->GetVolume();
	volume_hash[ volume->GetName() ]++;
	
	sum+=1; // Add one node
	sum+=countDaughters(node); // Count daughters
      }
  }
  return sum;
}

Int_t countNodes( const Char_t *name )
{
  // Given the volume name, recurse down the tree
  // and count all of the nodes and nodes of nodes...
  TGeoVolume *volume = gGeoManager->FindVolumeFast(name);
  assert(volume);

  volume_hash[ volume->GetName() ]++;

  TIter next(volume->GetNodes());
  TGeoNode *node = 0;
  Int_t sum = 0;
  while ((node=(TGeoNode*)next()))
    {
      volume = node->GetVolume();
      volume_hash[ volume->GetName() ]++;

      sum += 1; // Add one node
      sum += countDaughters( node ); // Count daughters
    }

  return sum;

}
/////////////////////////////////////////////////////////////////////////// geometryStats
void listVolumes( const Char_t *select  ) // = "ALL"
{
  std::map< TString, Int_t >::iterator iter = volume_hash.begin();
  while ( iter != volume_hash.end() )
    {
      TString name=(*iter).first;
      Int_t   count=(*iter).second;
      if ( TString(select) == "ALL" )
	std::cout << Form("%4s count=%03i",name.Data(),count) << std::endl;
      else if ( name==select )
	std::cout << Form("%4s count=%03i",name.Data(),count) << std::endl;
      iter++;
    }
}
/////////////////////////////////////////////////////////////////////////// geometryStats
void geometryStats( const Char_t *name )
{

  volume_hash.clear();

  TString myname=name;
  if ( myname=="FPDM" )          // Special case for FPD/FMS
    {
      geometryStats("FBOX");
      geometryStats("FBO1");
      geometryStats("FBO2");
      return;
    }
  if ( myname=="FTRO" )
    {
      geometryStats("FTMO");
      return;
    }


  Int_t nodes   = countNodes(name);
  Int_t volumes = volume_hash.size();

  numberOfNodes = nodes;
  numberOfVolumes = volumes;

#if 1
  std::cout << "Node and volume count for " << name << std::endl;
  std::cout                                       << 
    Form("+ N nodes      : %i",nodes)   << std::endl <<
    Form("+ N volumes    : %i",volumes) << std::endl <<
    std::endl
    ;
#endif
}
