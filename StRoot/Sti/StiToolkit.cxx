#include "StiToolkit.h"
#include "StiMaker/StiDefaultToolkit.h"

StiToolkit * StiToolkit::sInstance = 0;

StiToolkit * StiToolkit::instance()
{
	return (sInstance) ? sInstance :  new StiDefaultToolkit();
}

void StiToolkit::kill()
{
	if (sInstance)
		{
			delete sInstance;
			sInstance = 0;
		}
}
