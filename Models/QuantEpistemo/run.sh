cd /home/ubuntu/ComplexSystems/Perspectivism/Models/QuantEpistemo
./kill.sh
java -jar torpool.jar 100 9050 --mongo &
sleep 60s
./parrun.sh "java -jar bibliodata.jar --citation --mongo persp 5 1 true" 10

