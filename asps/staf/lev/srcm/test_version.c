#include <stdio.h>

int main(int argc, char **argv){
	static char id[]="@(#)$Id: test_version.c,v 1.1 1997/06/23 18:58:09 tull Exp $";
	printf("ID = %s \n",id);
	return 0;
}
