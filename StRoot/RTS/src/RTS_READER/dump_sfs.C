#include <stdio.h>
#include <fcntl.h>

#include <SFS/sfs_index.h>
#include <rtsLog.h>

#ifdef __ROOT__

#else



extern int dump_sfs(char *fname) ;

int main(int argc, char *argv[])
{
	if(argc == 2) {
		dump_sfs(argv[1]) ;
	}

	return 0 ;
}



#endif

static int dump_dir(class sfs_index *sfs, char *name)
{
	fs_dir *dir ;
	fs_dirent *dirent ;

	dir = sfs->opendir(name) ;
	if(dir==0) return 0 ;

	for(;;) {
		dirent = sfs->readdir(dir) ;
		if(dirent) {
			printf("%s\t\t%d\n",dirent->full_name,dirent->sz) ;
			if(dirent->sz==0) dump_dir(sfs,dirent->full_name) ;
		}
		else break ;
	} 

	return 0 ;
}

int dump_sfs(char *fname)
{
	sfs_index sfs ;
//	sfs.debug = 1 ;
	if(sfs.mount(fname,O_RDONLY,0666)<0) return -1 ;

	dump_dir(&sfs,"/") ;


	return 0 ;
}

