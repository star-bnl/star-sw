#include <string.h>
#include "asuAlloc.h"
#include "asuLib.h"
#include "emlLib.h"
#include "dio_types.h"

#define NAME_D "i_changed"
#define FILE_D "./output_test.xdf"
#define MODE_D DIO_WRITE_MODE

int dio_tcltk_browsefile(char** name, char** file, DIO_MODE_T *iomode)
{
	*name = (char*)MALLOC(strlen(NAME_D)+1);
	strcpy(*name,NAME_D);
	*file = (char*)MALLOC(strlen(FILE_D)+1);
	strcpy(*file,FILE_D);
	*iomode = MODE_D;
	return 1;
}

