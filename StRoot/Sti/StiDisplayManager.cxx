#include "StiDisplayManager.h"

StiDisplayManager * StiDisplayManager::sinstance = 0;

void StiDisplayManager::kill()
{  
  if (sinstance) 
    {
      delete sinstance;
      sinstance = 0;
    }
  return;
}
