ps -ef | grep bibliodata | awk -F" " '{print "kill -9 "$2}' | sh

java -jar bibliodata.jar --database --notproc persp

