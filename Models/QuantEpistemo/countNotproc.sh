echo `mongo persp --quiet --eval 'db.references.find({"citingFilled":false,"depth":{$gt:0},"processing":false}).count()'`
