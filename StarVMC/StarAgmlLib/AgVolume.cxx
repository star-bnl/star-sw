#include "AgVolume.h"
#include <iostream>

#include "AgBlock.h"
#include "AgModule.h"

std::map<TString, AgVolume *> AgVolume::mVolumeTable;

// -------------------------------------------------------------------------------------------------------------
AgVolume::AgVolume(const Char_t *name, const Char_t *title ):TNamed(name,title)
{

}
// -------------------------------------------------------------------------------------------------------------
AgVolume *AgVolume::Make()
{ 
  AgBlock  *block  = AgBlock::previous();
  AgModule *module = block->module();

  std::cout << Form("Creating new volume (or returning old one) for block %s in module %s",
		    block->GetName(),module->GetName()) << std::endl;

  return NULL;
}
