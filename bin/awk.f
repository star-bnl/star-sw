BEGIN { }
#if (index("sys/D0_machine.h",$0)==0 && index("pilot.h",$0)==0) 
i2 = index("pilot.h",$0)
i1 = index("D0_machine.h",$0)
{print i1, i2, $0}
END { }
