cd /home/ubuntu/ComplexSystems/Perspectivism/Models/QuantEpistemo

TOTAL=`mongo persp --quiet --eval "db.references.count()"`
LINKS=`mongo persp --quiet --eval "db.links.count()"`
REMAINING=`mongo persp --quiet --eval 'db.references.find({"citingFilled":false,"depth":{$gt:0}}).count()'`
DATE=`date +%s`

echo "$TOTAL;$LINKS;$REMAINING"

echo "$DATE;$TOTAL;$LINKS;$REMAINING" >> progress.txt
