/* tu_machine_type.c  defines which type of machine is running.
 *
 * Created by D. Olson, 10-July-1992
 *
 */

#ifdef SUN

#define MACH_TYPE "SUN4"
#define MACH_NUMB 0

int tu_machine_type_( type, len )
     char *type;
     int len;
{
    strncpy( type, MACH_TYPE, len );
    return MACH_NUMB;
}

#endif /* SUN */

#ifdef VAX

#include <descrip.h>

#define MACH_TYPE "VAX"
#define MACH_NUMB 1

int tu_machine_type( type )
     struct dsc$descriptor *type;
{
    strncpy( type->dsc$a_pointer, MACH_TYPE, type->dsc$w_length );
    return MACH_NUMB;
}

#endif /* VAX */

#ifdef HPUX

#define MACH_TYPE "HPUX"
#define MACH_NUMB 2

int tu_machine_type_( type, len )
     char *type;
     int len;
{
    strncpy( type, MACH_TYPE, len );
    return MACH_NUMB;
}

#endif /* HPUX */

#ifdef IRIX

#define MACH_TYPE "IRIX"
#define MACH_NUMB 3

int tu_machine_type_( type, len )
     char *type;
     int len;
{
    strncpy( type, MACH_TYPE, len );
    return MACH_NUMB;
}

#endif /* IRIX */
