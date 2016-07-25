#! /usr/local/bin/tcsh -f
#set dirlist = "/star/mds/data/SD98/auau200/hijing135/ /net/rmds03/disk1/star/auau200/hijing135/ /net/rmds03/disk1/star/auau200/hijing135/"
#set dirlist = "/star/mds/data/SD98/auau200/bfc/ /star/mds/data/SD98/auau200/evg/ /star/mds/data/SD98/auau200/fast/ /star/mds/data/SD98/auau200/fast2/ /star/mds/data/SD98/auau200/fast_new/ /star/mds/data/SD98/auau200/g2t/ /star/mds/data/SD98/auau200/gst"
set dirlist = ""
set PWD = `pwd`
touch $PWD/tmp.tmp
foreach dir ($dirlist) 
  cd $dir
  find . -name "*.fz*" -exec ls -alFs {} \; | gawk -v V=${dir} '{print $10"\t"V"\t"$1"\t"$6" "$7" "$8" "$9}' >> $PWD/tmp.tmp
  find . -name "*.xd*" -exec ls -alFs {} \; | gawk -v V=${dir} '{print $10"\t"V"\t"$1"\t"$6" "$7" "$8" "$9}' >> $PWD/tmp.tmp
end
set dirlist = "/home/starsink/raw/auau200/hijing135/"
#set dirlist = "/home/starsink/raw/auau200/bfc/"
foreach dir ($dirlist)
#  ftp -i -v rmds01 2121 <<EOF 
#  cd $dir
#  mdir */*/*/*/*/*.*  h1.list 
#EOF
 set filelist = `more h1.list`
 while ($#filelist>0)
#   more h1.list |  gawk -v V=${dir} '{print $0"\t"V}' >> $PWD/tmp.tmp
   set file = $filelist[1];   shift filelist
   hpss_dir -d $dir -f $file

 end
end

#sed -e 's/\.\///g' tmp.tmp | sort -u > sorted.list
