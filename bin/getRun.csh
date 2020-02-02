 echo "select max(runNumber) as runNumber from runDescriptor where beginTime>'2001-08-01 00:00:00'" | mysql -h onlsun1.star.bnl.gov --port=3501 -C RunLog
